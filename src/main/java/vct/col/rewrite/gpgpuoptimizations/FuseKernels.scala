package vct.col.rewrite.gpgpuoptimizations

import hre.ast.MessageOrigin
import hre.lang.System.Output
import vct.col.ast.`type`.{ASTReserved, PrimitiveSort}
import vct.col.ast.`type`.PrimitiveSort._
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.expr.{MethodInvokation, NameExpression, OperatorExpression}
import vct.col.ast.expr.constant.{ConstantExpression, IntegerValue}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement, ParallelBlock, ParallelRegion}
import vct.col.ast.stmt.decl.{ASTSpecial, Contract, Method, ProgramUnit}
import vct.col.ast.util.{ASTUtils, AbstractRewriter, ContractBuilder, NameScanner, RecursiveVisitor, SequenceUtils}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.language.postfixOps

class FuseKernels(override val source: ProgramUnit) extends AbstractRewriter(source) {

  var inMethod = false
  // Original Method name -> Dependency Check methods
  //TODO OS can we use the method ASTNode as a key
  val methodsWithFusion: mutable.Map[String, mutable.Buffer[Method]] = mutable.Map.empty.withDefaultValue(mutable.Buffer.empty)


  override def visit(m: Method): Unit = {
    inMethod = true
    super.visit(m)
    inMethod = false
  }

  override def visit(s: BlockStatement): Unit = {
    //TODO OS exclude loops and other ASTNodes using BlockStatement
    //TODO OS excluse the contract of the method
    if (!inMethod || (s.getParent != null && !s.getParent.isInstanceOf[Method])) {
      super.visit(s)
      return
    }
    var nf = 0
    var parBlocks = Seq.empty[ParallelRegion]
    val newBlock = create.block()
    for (st <- s.getStatements) {
      st match {
        case pr: ParallelRegion if pr.fuse != null => {
          val f = pr.fuse.F.value.asInstanceOf[IntegerValue].value
          if (f == 0) {
            Warning("Number of fusions is zero, no optimization applied.", pr.fuse.getOrigin)
          } else {
            nf = math.max(nf - 1, f)
            parBlocks ++= Seq(pr)
          }
        }
        case pr: ParallelRegion => {
          if (nf != 0) {
            nf = math.max(nf - 1, 0)
            parBlocks ++= Seq(pr)
            if (nf == 0) {
              newBlock.add(fuseParBlocks(parBlocks))
            }
          } else {
            newBlock.add(copy_rw.rewrite(pr))
          }
        }
        case stmt => {
          if (nf != 0) {
            Fail("No statements allowed between parallel regions while fusion", stmt.getOrigin)
          }
          //TODO OS on which statements should it fail and which not?
          newBlock.add(rewrite(stmt))
        }
      }
    }
    if (nf != 0) {
      //      Fail("There are not enough kernels to fuse as specified in one of the gpuopt annotations")
    }
    result = newBlock

  }

  private def fuseParBlocks(parBlocks: Seq[ParallelRegion]) = {

    //TODO OS check whether each region only has one block
    //TODO OS check whether each one block only had one iter

    ////////////////////////////////
    /// Check bounds of all tids ///
    ////////////////////////////////
    val distinctBounds = parBlocks.map(_.blocks.head.iters.head.init.get.asInstanceOf[OperatorExpression])
      .map(n => (n.first, n.second))
      .distinct

    if (distinctBounds.size > 1) {
      Fail("All kernels (and parallel blocks) to fuse must have the same thread configuration")
    }

    ////////////////////////////
    /// Initiliaze variables ///
    ////////////////////////////
    val newParBody = create.block()
    val newTid = name("vct_fused_tid")

    ///////////////////
    /// Fuse bodies ///
    ///////////////////
    val parblocks = parBlocks
      .map(p => ASTUtils.replace(name(p.blocks.head.iters.head.name), newTid, p).asInstanceOf[ParallelRegion])
      .zipWithIndex
      .map { pbi =>
        val newBlock = create.block()
        pbi._1.blocks.head.block match {
          case null =>
          case b: BlockStatement =>
            b.forEachStmt(st => newBlock.add(rewrite(st)))
          case elseNode => newBlock.add(rewrite(elseNode))
        }
        if (pbi._2 < parBlocks.length - 1) {
          ASTUtils.conjuncts(pbi._1.blocks.head.contract.post_condition, Star, And, Wrap).forEach { stmt =>
            newBlock.add(create special(ASTSpecial.Kind.Assert, copy_rw.rewrite(stmt)))
          }
        }
        newBlock
      }
    parblocks.foreach(_.forEachStmt(l => newParBody.add(rewrite(l))))


    //////////////////////
    /// Fuse contracts ///
    //////////////////////
    var kernelZeroToI = parBlocks.head
    for (i <- 0 to parBlocks.length - 2) {
      val kernelIPlusOne = parBlocks(i + 1)

      val cbPermsI = new ContractBuilder()
      val cbNonPermI = new ContractBuilder()
      val cbPermsIPlusOne = new ContractBuilder()
      val cbNonPermsIPlusOne = new ContractBuilder()
      ASTUtils.conjuncts(kernelZeroToI.blocks.head.contract.pre_condition, Star, And, Wrap).forEach { stmt =>
        val cp = new CollectPerms()
        cp.rewrite(stmt)
        if (cp.perm.isDefined) {
          cbPermsI.requires(stmt)
        } else {
          cbNonPermI.requires(stmt)
        }
      }
      ASTUtils.conjuncts(kernelZeroToI.blocks.head.contract.post_condition, Star, And, Wrap).forEach { stmt =>
        val cp = new CollectPerms()
        cp.rewrite(stmt)
        if (cp.perm.isDefined) {
          cbPermsI.ensures(stmt)
        } else {
          cbNonPermI.ensures(stmt)
        }
      }

      ASTUtils.conjuncts(kernelIPlusOne.blocks.head.contract.pre_condition, Star, And, Wrap).forEach { stmt =>
        val cp = new CollectPerms()
        cp.rewrite(stmt)
        if (cp.perm.isDefined) {
          cbPermsIPlusOne.requires(stmt)
        } else {
          cbNonPermsIPlusOne.requires(stmt)
        }
      }
      ASTUtils.conjuncts(kernelIPlusOne.blocks.head.contract.post_condition, Star, And, Wrap).forEach { stmt =>
        val cp = new CollectPerms()
        cp.rewrite(stmt)
        if (cp.perm.isDefined) {
          cbPermsIPlusOne.ensures(stmt)
        } else {
          cbNonPermsIPlusOne.ensures(stmt)
        }
      }

      val contractIPerm =
        ASTUtils.replace(name(kernelZeroToI.blocks.head.iters.head.name), newTid, cbPermsI.getContract(false)).asInstanceOf[Contract]
      val contractIPlusOnePerm =
        ASTUtils.replace(name(kernelIPlusOne.blocks.head.iters.head.name), newTid, cbPermsIPlusOne.getContract(false)).asInstanceOf[Contract]
      val contractINonPerm =
        ASTUtils.replace(name(kernelZeroToI.blocks.head.iters.head.name), newTid, cbNonPermI.getContract(false)).asInstanceOf[Contract]
      val contractIPlusOneNonPerm =
        ASTUtils.replace(name(kernelIPlusOne.blocks.head.iters.head.name), newTid, cbNonPermsIPlusOne.getContract(false)).asInstanceOf[Contract]


      ////////////////////////////////////////
      /// Calculate shared/non-shared sets ///
      ////////////////////////////////////////
      val freeVarsI = NameScanner.freeVars(contractIPerm.pre_condition).asScala
        .filter(!_._1.equals(newTid.name))
        .filter(kv => SequenceUtils.getTypeInfo(kv._2) == null || SequenceUtils.getTypeInfo(kv._2).getSequenceSort.equals(Array))

      val freeVarsIplusone = NameScanner.freeVars(contractIPlusOnePerm.pre_condition).asScala
        .filter(!_._1.equals(newTid.name))
        .filter(kv => SequenceUtils.getTypeInfo(kv._2) == null || SequenceUtils.getTypeInfo(kv._2).getSequenceSort.equals(Array))

      val varsI = freeVarsI.keySet
      val varsIplusone = freeVarsIplusone.keySet

      val shared = varsI.intersect(varsIplusone)
      val nonshared = ((varsI diff varsIplusone) union (varsIplusone diff varsI))

      /////////////////////////////
      /// Data dependency check ///
      /////////////////////////////
      //TODO OS look at the data dependency check method again
      val cbDepCheck = new ContractBuilder()
      val cbFusedParBlock = new ContractBuilder()

      nonshared.foreach { nonsharedVar =>
        var foundAny = false
        ASTUtils.conjuncts(contractIPerm.pre_condition, Star, And, Wrap).forEach { stmt =>
          if (ASTUtils.find_name(stmt, nonsharedVar)) {
            cbDepCheck.requires(rewrite(stmt))
            cbFusedParBlock.requires(rewrite(stmt))
            foundAny = true
          }
        }
        if (!foundAny) {
          ASTUtils.conjuncts(contractIPlusOnePerm.pre_condition, Star, And, Wrap).forEach { stmt =>
            if (ASTUtils.find_name(stmt, nonsharedVar)) {
              cbDepCheck.requires(rewrite(stmt))
              cbFusedParBlock.requires(rewrite(stmt))
            }
          }
        }
      }

      shared.foreach { sharedVar =>
        ASTUtils.conjuncts(contractIPerm.pre_condition, Star, And, Wrap).forEach { stmt =>
          if (ASTUtils.find_name(stmt, sharedVar)) {
            cbDepCheck.requires(rewrite(stmt))
            cbFusedParBlock.requires(rewrite(stmt))
          }
        }
      }

      nonshared.foreach { nonsharedVar =>
        var foundAny = false
        ASTUtils.conjuncts(contractIPerm.post_condition, Star, And, Wrap).forEach { stmt =>
          if (ASTUtils.find_name(stmt, nonsharedVar)) {
            cbFusedParBlock.ensures(rewrite(stmt))
            foundAny = true
          }
        }
        if (!foundAny) {
          ASTUtils.conjuncts(contractIPlusOnePerm.post_condition, Star, And, Wrap).forEach { stmt =>
            if (ASTUtils.find_name(stmt, nonsharedVar)) {
              cbFusedParBlock.ensures(rewrite(stmt))
            }
          }
        }
      }



      val permPatternsIPlusOne = ASTUtils.conjuncts(contractIPlusOnePerm.pre_condition, Star, And, Wrap).asScala
        .filter(pre => shared.exists(ASTUtils.find_name(pre, _)))
        .map { pre =>
          val cp = new CollectPerms()
          cp.rewrite(pre)
          val res = cp.perm.get
          // (Pattern -> (permission, original precondition))
          (rewrite(res.first), (rewrite(res.second), rewrite(pre)))
        }
        .groupBy(_._1)
        .map(kv => (kv._1, kv._2.map(kv2 => kv2._2)))
        .map { kv =>
          val value = kv._2.foldLeft((Seq.empty[ASTNode], Seq.empty[ASTNode])) {
            (prev, next) =>
              (prev._1 :+ next._1, prev._2 :+ next._2)
          }
          (kv._1, value)
        }

      val permPatternsIPlusOnePost = ASTUtils.conjuncts(contractIPlusOnePerm.post_condition, Star, And, Wrap).asScala
        .filter(pre => shared.exists(ASTUtils.find_name(pre, _)))
        .map { pre =>
          val cp = new CollectPerms()
          cp.rewrite(pre)
          val res = cp.perm.get
          // (Pattern -> (permission, original precondition))
          (rewrite(res.first), (rewrite(res.second), rewrite(pre)))
        }
        .groupBy(_._1)
        .map(kv => (kv._1, kv._2.map(kv2 => kv2._2)))
        .map { kv =>
          val value = kv._2.foldLeft((Seq.empty[ASTNode], Seq.empty[ASTNode])) {
            (prev, next) =>
              (prev._1 :+ next._1, prev._2 :+ next._2)
          }
          (kv._1, value)
        }

      val permPatternsI = ASTUtils.conjuncts(contractIPerm.pre_condition, Star, And, Wrap).asScala
        .filter(pre => shared.exists(ASTUtils.find_name(pre, _)))
        .map { pre =>
          val cp = new CollectPerms()
          cp.rewrite(pre)
          val res = cp.perm.get
          (rewrite(res.first), (rewrite(res.second), rewrite(pre)))
        }
        .groupBy(_._1)
        .map(kv => (kv._1, kv._2.map(kv2 => kv2._2)))
        .map { kv =>
          val value = kv._2.foldLeft((Seq.empty[ASTNode], Seq.empty[ASTNode])) {
            (prev, next) =>
              (prev._1 :+ next._1, prev._2 :+ next._2)
          }
          (kv._1, value)
        }

      val permPatternsIPost = ASTUtils.conjuncts(contractIPerm.post_condition, Star, And, Wrap).asScala
        .filter(pre => shared.exists(ASTUtils.find_name(pre, _)))
        .map { pre =>
          val cp = new CollectPerms()
          cp.rewrite(pre)
          val res = cp.perm.get
          (rewrite(res.first), (rewrite(res.second), rewrite(pre)))
        }
        .groupBy(_._1)
        .map(kv => (kv._1, kv._2.map(kv2 => kv2._2)))
        .map { kv =>
          val value = kv._2.foldLeft((Seq.empty[ASTNode], Seq.empty[ASTNode])) {
            (prev, next) =>
              (prev._1 :+ next._1, prev._2 :+ next._2)
          }
          (kv._1, value)
        }

      permPatternsIPlusOne.foreach { case (patt, (perms, conditions)) =>
        if (!permPatternsIPost.contains(patt)) { // Do 5.1
          // Precondition     Permissions
          conditions.foreach { s =>
            cbDepCheck.requires(rewrite(s))
            cbFusedParBlock.requires(rewrite(s))
          }
          // Precondition     Functional
          // TODO OS

          // Postcondition    Permissions
          // Do 2.2.1.1
          val sharedVars = NameScanner.freeVars(patt).asScala.filter(!_._1.equals(newTid.name))
          sharedVars.foreach { case (varName, varType) =>
            permPatternsIPost.foreach { case (pattIPost, (permsIPlusOnePost, conditionsIPost)) =>
              if (ASTUtils.find_name(pattIPost, varName)) {
                //TODO OS can you get duplicates
                conditionsIPost.foreach(st => cbFusedParBlock.ensures(copy_rw.rewrite(st)))
              }
            }
            permPatternsIPlusOnePost.foreach { case (pattIPlusOnePost, (_, conditionsIPlusOnePost)) =>
              if (ASTUtils.find_name(pattIPlusOnePost, varName)) {
                conditionsIPlusOnePost.foreach(st => cbFusedParBlock.ensures(copy_rw.rewrite(st)))
              }
            }
          }

          // Postcondition    Functional
          // TODO OS
        } else if (perms.map(interpretPermission).sum == permPatternsIPost(patt)._1.map(interpretPermission).sum) { // Do 5.2
          // Precondition     Permissions
          // Do nothing
          // Precondition     Functional
          // TODO OS
          // Postcondition    Permissions
          // See below
          // Postcondition    Functional
          // TODO OS
        } else if (perms.map(interpretPermission).sum < permPatternsIPost(patt)._1.map(interpretPermission).sum) { // Do 5 .3
          // Precondition     Permissions
          // Do nothing
          // Precondition     Functional
          // TODO OS
          // Postcondition    Permissions
          // See below
          // Postcondition    Functional
          // TODO OS
        } else if (perms.map(interpretPermission).sum > permPatternsIPost(patt)._1.map(interpretPermission).sum) { // Do 5.4
          // Precondition     Permissions
          val additionalPerm = rewrite(minus(
            perms.reduce((lhs, rhs) => plus(rewrite(lhs), rewrite(rhs))),
            permPatternsIPost(patt)._1.reduce((lhs, rhs) => plus(rewrite(lhs), rewrite(rhs)))
          )
          )
          val newRequires = create.expression(Perm, rewrite(patt), additionalPerm)
          cbDepCheck.requires(newRequires)
          cbFusedParBlock.requires(newRequires)
          // Precondition     Functional
          // TODO OS
          // Postcondition    Permissions
          // See below
          // Postcondition    Functional
          // TODO OS
        }

        if (permPatternsIPost.contains(patt)) {
          // Postcondition    Permissions
          // So case 5.2, 5.3 or 5.4
          // Do 2.2.1.2
          val sharedVars = NameScanner.freeVars(patt).asScala.filter(!_._1.equals(newTid.name))
          val permPatternsFusedPre = ASTUtils.conjuncts(cbFusedParBlock.getContract(false).pre_condition, Star, And, Wrap).asScala
            .filter(pre => sharedVars.keySet.exists(ASTUtils.find_name(pre, _)))
            .map { pre =>
              val cp = new CollectPerms()
              cp.rewrite(pre)
              val res = cp.perm.get
              // (Pattern -> (permission, original precondition))
              (rewrite(res.first), rewrite(res.second))
            }
            .groupBy(_._1)
            .map(kv => (kv._1, kv._2.map(kv2 => kv2._2)))
            .map { kv =>
              val value = kv._2.foldLeft(Seq.empty[ASTNode]) { (prev, next) => prev :+ next }
              (kv._1, value)
              // (Pattern -> [permission]
            }

          //Do 2.2.1.
          sharedVars.foreach { sharedVar =>
            val sharedIPlusOnePost = permPatternsIPlusOnePost
              .filter { case (pattern, (perms, conditions)) => ASTUtils.find_name(pattern, sharedVar._1) }

            //Do 2.2.1.2.2
            val totalPermsFusedPre = permPatternsFusedPre
              .filter { case (pattern, _) => ASTUtils.find_name(pattern, newTid.name) && ASTUtils.find_name(pattern, sharedVar._1) }
              .map { case (patt, perms) => (patt, perms.reduce((lhs, rhs) => plus(rewrite(lhs), rewrite(rhs)))) }
              .values
              .reduce((lhs, rhs) => plus(lhs, rhs))

            //Do 2.2.1.2.3

            val totalPermIPre = permPatternsI
              .filter { case (pattern, _) => ASTUtils.find_name(pattern, newTid.name) && ASTUtils.find_name(pattern, sharedVar._1) }
              .map { case (patt, (perms, _)) => (patt, perms.reduce((lhs, rhs) => plus(rewrite(lhs), rewrite(rhs)))) }
              .values
              .reduce((lhs, rhs) => plus(lhs, rhs))
            val totalPermIPost = permPatternsIPost
              .filter { case (pattern, _) => ASTUtils.find_name(pattern, newTid.name) && ASTUtils.find_name(pattern, sharedVar._1) }
              .map { case (patt, (perms, _)) => (patt, perms.reduce((lhs, rhs) => plus(rewrite(lhs), rewrite(rhs)))) }
              .values
              .reduce((lhs, rhs) => plus(lhs, rhs))

            val totalPermIPlusOnePre = permPatternsIPlusOne
              .filter { case (pattern, _) => ASTUtils.find_name(pattern, newTid.name) && ASTUtils.find_name(pattern, sharedVar._1) }
              .map { case (patt, (perms, _)) => (patt, perms.reduce((lhs, rhs) => plus(rewrite(lhs), rewrite(rhs)))) }
              .values
              .reduce((lhs, rhs) => plus(lhs, rhs))

            val totalPermIPlusOnePost = permPatternsIPlusOnePost
              .filter { case (pattern, _) => ASTUtils.find_name(pattern, newTid.name) && ASTUtils.find_name(pattern, sharedVar._1) }
              .map { case (patt, (perms, _)) => (patt, perms.reduce((lhs, rhs) => plus(rewrite(lhs), rewrite(rhs)))) }
              .values
              .reduce((lhs, rhs) => plus(lhs, rhs))

            val diffI = minus(totalPermIPre, totalPermIPost)
            val diffIPlusOne = minus(totalPermIPlusOnePre, totalPermIPlusOnePost)
            val lostPerms = plus (diffI, diffIPlusOne)
            val newPerm = minus(totalPermsFusedPre, lostPerms)

            sharedIPlusOnePost.foreach { case (pattern, (_, permStmts)) =>
              permStmts.foreach { permStmt =>
                cbFusedParBlock.ensures(replacePerm(pattern, newPerm, permStmt))
              }
            }
          }
        }
      }


      val depCheckParBlock = create.parallel_block(
        "vct_fused_dep_check",
        cbDepCheck.getContract(),
        scala.Array(create.field_decl(newTid.name, create.primitive_type(PrimitiveSort.Integer), rewrite(parBlocks.head.blocks.head.iters.head.init.get))).toList.asJava,
        create.block(),
        scala.Array.empty[ASTNode].toSeq.asJava.toArray(scala.Array.empty[ASTNode])
      )
      val depCheckParRegion = create.region(null, null, depCheckParBlock)
      val cbDepCheckMethod = new ContractBuilder()
      cbDepCheckMethod.requires(copy_rw.rewrite(current_method().getContract().pre_condition))
      cbDepCheckMethod.`given`(copy_rw.rewrite(current_method().getContract().`given`): _*)
      cbDepCheckMethod.yields(copy_rw.rewrite(current_method().getContract().yields): _*)
      cbDepCheckMethod.appendInvariant(copy_rw.rewrite(current_method().getContract().invariant))

      val depCheckMethod = create.method_decl(
        create.primitive_type(PrimitiveSort.Void),
        cbDepCheckMethod.getContract,
        "vct_dependency_check_" + current_method().name,
        copy_rw.rewrite(current_method().getArgs),
        create.block().add(depCheckParRegion)
      )
      depCheckMethod.setStatic(false)

      Output("%s", depCheckMethod)

      //      methodsWithFusion(current_method().name) = methodsWithFusion(current_method().name) ++ mutable.Buffer(dataDepCheckMethod)

      ///////////////////////////
      /// Create fused kernel ///
      ///////////////////////////
      val newIter = create.field_decl(newTid.name, create.primitive_type(PrimitiveSort.Integer), rewrite(parBlocks.head.blocks.head.iters.head.init.get))
      val newLabel = "vct_fused_kernels"
      val newParBlock = create parallel_block(
        newLabel,
        cbFusedParBlock.getContract(false),
        scala.Array(newIter).toList.asJava,
        newParBody,
        null
      )
      kernelZeroToI = create.region(null, null, newParBlock)
    }


    kernelZeroToI
  }

  def replacePerm(pattern: ASTNode, newPerm: ASTNode, tree: ASTNode): ASTNode = {
    val rw = new AbstractRewriter(null.asInstanceOf[ProgramUnit]) {
      override def visit(e: OperatorExpression): Unit = {
        if (e.operator == Perm)
          result = create.expression(Perm, rewrite(e.first), copy_rw.rewrite(newPerm))
        else super.visit(e)
      }
    }
    rw.rewrite(tree)
  }

  def interpretPermission(perm: ASTNode): scala.Float = {
    perm match {
      case n: NameExpression if n.isReserved(ASTReserved.FullPerm) => 1
      case n: NameExpression if n.isReserved(ASTReserved.NoPerm) => 0
      case o: OperatorExpression if o.operator == Div => interpretPermission(o.first) / interpretPermission(o.second)
      case c: ConstantExpression if c.value.isInstanceOf[IntegerValue] => c.value.asInstanceOf[IntegerValue].value
      case anyelse =>
        Fail("Could not interpret %s as a concrete permission.", anyelse)
        return -1
    }
  }

  case class CollectPerms() extends AbstractRewriter(null: ProgramUnit) {
    var perm: Option[OperatorExpression] = None

    override def visit(e: OperatorExpression): Unit = {
      e.operator match {
        case Perm =>
          perm = Some(copy_rw.rewrite(e))
        case _ =>
      }
      super.visit(e)
    }
  }

}
