package vct.col.rewrite.gpgpuoptimizations

import scala.util.control.Breaks._
import hre.lang.System.Output
import vct.col.ast.`type`.{ASTReserved, PrimitiveSort}
import vct.col.ast.`type`.PrimitiveSort._
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.expr.{Dereference, MethodInvokation, NameExpression, OperatorExpression}
import vct.col.ast.expr.constant.{ConstantExpression, IntegerValue}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement, ParallelBarrier, ParallelBlock, ParallelRegion}
import vct.col.ast.stmt.decl.{ASTSpecial, Contract, Method, ProgramUnit}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.{ASTUtils, AbstractRewriter, ContractBuilder, NameScanner, RecursiveVisitor, SequenceUtils}
import scala.collection.mutable
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
            newBlock.add(rewrite(pr))
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
    val newTid = name("vct_fused_tid")
    val newLabel = "vct_fused_kernels"

    //////////////////////
    /// Fuse contracts ///
    //////////////////////
    var depCheckMethod: Method = null
    var kernelZeroToI = parBlocks.head
    for (i <- 0 to parBlocks.length - 2) {
      val newParBody = create.block()

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

      /////////////////////////////////////////
      /// Calculate SV for each shared var  ///
      /////////////////////////////////////////
      val arrayToLength = shared
        .map(sharedVar => (sharedVar,
          findConcreteValue(
            name(sharedVar),
            star(current_method().getContract().pre_condition, current_method().getContract().invariant))
        )
        )
        .toMap
      Output(arrayToLength.toString())

      val rangeOfTid = kernelIPlusOne.blocks.head.iters.head.init.get.asInstanceOf[OperatorExpression]

      val T = getConstantInteger(rangeOfTid.second).getOrElse(findConcreteValue(rangeOfTid.second, star(current_method().getContract().pre_condition, current_method().getContract().invariant)))
      -getConstantInteger(rangeOfTid.first).getOrElse(findConcreteValue(rangeOfTid.first, star(current_method().getContract().pre_condition, current_method().getContract().invariant)))
      Output(T.toString)
      val sharedVarToSVI = SV(contractIPerm.pre_condition, arrayToLength, T, newTid)
      val sharedVarToSVIPlusOne = SV(contractIPlusOnePerm.pre_condition, arrayToLength, T, newTid)

      /////////////////////////////
      /// Data dependency check ///
      /////////////////////////////
      //TODO OS look at the data dependency check method again
      val cbFusedParBlock = new ContractBuilder()

      nonshared.foreach { nonsharedVar =>
        // Precondition for permissions
        var foundAny = false
        ASTUtils.conjuncts(contractIPerm.pre_condition, Star, And, Wrap).forEach { stmt =>
          if (ASTUtils.find_name(stmt, nonsharedVar)) {
            cbFusedParBlock.requires(rewrite(stmt))
            foundAny = true
          }
        }
        if (!foundAny) {
          ASTUtils.conjuncts(contractIPlusOnePerm.pre_condition, Star, And, Wrap).forEach { stmt =>
            if (ASTUtils.find_name(stmt, nonsharedVar)) {
              cbFusedParBlock.requires(rewrite(stmt))
            }
          }
        }
        // Precondition for functional properties
        ASTUtils.conjuncts(contractINonPerm.pre_condition, Star, And, Wrap).forEach { preNonPerm =>
          if (ASTUtils.find_name(preNonPerm, nonsharedVar) && !shared.exists(ASTUtils.find_name(preNonPerm, _))) {
            cbFusedParBlock.requires(rewrite(preNonPerm))
          }
        }
        ASTUtils.conjuncts(contractIPlusOneNonPerm.pre_condition, Star, And, Wrap).forEach { preNonPerm =>
          if (ASTUtils.find_name(preNonPerm, nonsharedVar) && !shared.exists(ASTUtils.find_name(preNonPerm, _))) {
            cbFusedParBlock.requires(rewrite(preNonPerm))
          }
        }
      }

      //TODO OS you can simplify this by moving shared.foreach to an shared.exists in the condition
      // Also helps against duplication
      //      shared.foreach { sharedVar =>
      //        ASTUtils.conjuncts(contractIPerm.pre_condition, Star, And, Wrap).forEach { stmt =>
      //          if (ASTUtils.find_name(stmt, sharedVar) && ASTUtils.find_name(stmt, newTid.name)) {
      //            cbFusedParBlock.requires(rewrite(stmt))
      //          }
      //        }
      //      }


//      ASTUtils.conjuncts(contractINonPerm.pre_condition, Star, And, Wrap).forEach { preNonPerm =>
//        if (shared.exists(ASTUtils.find_name(preNonPerm, _))) {
//          cbFusedParBlock.requires(rewrite(preNonPerm))
//        }
//      }

      // 2.1.1
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

      // Postcondition for non-shared functional properties
      ASTUtils.conjuncts(contractINonPerm.post_condition, Star, And, Wrap).forEach { postNonPerm =>
        if (nonshared.exists(ASTUtils.find_name(postNonPerm, _)) && !shared.exists(ASTUtils.find_name(postNonPerm, _))) {
          cbFusedParBlock.ensures(rewrite(postNonPerm))
        }
      }
      ASTUtils.conjuncts(contractIPlusOneNonPerm.post_condition, Star, And, Wrap).forEach { postNonPerm =>
        if (nonshared.exists(ASTUtils.find_name(postNonPerm, _)) && !shared.exists(ASTUtils.find_name(postNonPerm, _))) {
          cbFusedParBlock.ensures(rewrite(postNonPerm))
        }
      }

      val getPermPatterns = (i: ASTNode) => {
        ASTUtils.conjuncts(i, Star, And, Wrap).asScala.toSeq
          .filter(pre => shared.exists(ASTUtils.find_name(pre, _)))
          .map { pre =>
            val cp = new CollectPerms()
            cp.rewrite(pre)
            val res = cp.perm.get
            (rewrite(res.first), (rewrite(res.second), rewrite(pre)))
          }
          .groupBy { case (patt, _) =>
            NameScanner.freeVars(patt).asScala.filter(!_._1.equals(newTid.name)).keySet.head
          }

          //          .map(kv => (kv._1, kv._2.map(kv2 => kv2._2)))
          .map { kv =>
            val value = kv._2.foldLeft((Seq.empty[ASTNode], Seq.empty[ASTNode], Seq.empty[ASTNode])) {
              (prev, next) =>
                (prev._1 :+ next._1, prev._2 :+ next._2._1, prev._3 :+ next._2._2)
            }
            (kv._1, value)
          }
      }
        .map { case (sharedVar, (patts, perms, anns)) =>
          val normalizedPerms = perms
            .map {
              case ConstantExpression(value) if value.asInstanceOf[IntegerValue].value == 1 =>
                create.expression(Div, constant(1), constant(1))
              case o: OperatorExpression if o.operator == UMinus =>
                o.first match {
                  case ConstantExpression(value2) if value2.asInstanceOf[IntegerValue].value == 1 =>
                    create.expression(UMinus, create.expression(Div, constant(1), constant(1)))
                  case _ => o
                }
              case node => node
            }
          (sharedVar, (patts, normalizedPerms, anns))
        }

      val permPatternsIPlusOnePre = getPermPatterns(contractIPlusOnePerm.pre_condition)
      val permPatternsIPlusOnePost = getPermPatterns(contractIPlusOnePerm.post_condition)
      val permPatternsIPre = getPermPatterns(contractIPerm.pre_condition)
      val permPatternsIPost = getPermPatterns(contractIPerm.post_condition)

      val DP = mutable.Set.empty[String]

      permPatternsIPlusOnePre
        .foreach { case (sharedVarTmp, (patts, perms, conditions)) =>

          val pattsIPlusOne = patts.filter(ASTUtils.find_name(_, newTid.name))
          val pattsI = permPatternsIPost(sharedVarTmp)._1.filter(ASTUtils.find_name(_, newTid.name))


          val setOfDifferentPatterns = (pattsIPlusOne diff pattsI) union (pattsI diff pattsIPlusOne)

          if (setOfDifferentPatterns.nonEmpty) { // .1
            // Preconditions
            val SV_iplusone = SV(contractIPlusOnePerm.pre_condition, arrayToLength, T, newTid)
            val SV_i = SV(contractIPerm.pre_condition, arrayToLength, T, newTid)

            val accumulatedPerms = SV_i(sharedVarTmp).zip(SV_iplusone(sharedVarTmp)).map {
              case (lhs, rhs) => lhs.map(_._1) ++ rhs.map(_._1)
            }
              .map(_.map(interpretPermission).sum)

            if (accumulatedPerms.forall(_ <= 1)) { // .1.1
              SV_i(sharedVarTmp).flatMap(s => s.map(_._2)).distinct.foreach(st => cbFusedParBlock.requires(st))
              SV_iplusone(sharedVarTmp).flatMap(s => s.map(_._2)).distinct.foreach(st => cbFusedParBlock.requires(st))

              // Postconditions
              if (permPatternsIPost.contains(sharedVarTmp)) {
                permPatternsIPost(sharedVarTmp)._3.foreach { post =>
                  cbFusedParBlock.ensures(copy_rw.rewrite(post))
                }
              }
              if (permPatternsIPlusOnePost.contains(sharedVarTmp)) {
                permPatternsIPlusOnePost(sharedVarTmp)._3.foreach { post =>
                  cbFusedParBlock.ensures(copy_rw.rewrite(post))
                }
              }


            } else if (perms.forall(interpretPermission(_) < 1) &&
              permPatternsIPost(sharedVarTmp)._2.forall(interpretPermission(_) < 1)) { // .1.2
              val permsPattsAnnI = permPatternsIPre(sharedVarTmp)
              val C_i = permsPattsAnnI._1.count(ASTUtils.find_name(_, newTid.name))
              val C_iplusone = pattsIPlusOne.length
              val C = C_i + C_iplusone

              val newPerm = create.expression(Div, constant(1), constant(C))

              val annIPlusOne = conditions
              val annI = permsPattsAnnI._3

              annI.foreach { ann => cbFusedParBlock.requires(replacePerm(newPerm, ann)) }
              annIPlusOne.foreach { ann => cbFusedParBlock.requires(replacePerm(newPerm, ann)) }

              val C_ipost = permPatternsIPost(sharedVarTmp)._1.count(ASTUtils.find_name(_, newTid.name))
              val C_iplusonepost = permPatternsIPlusOnePost(sharedVarTmp)._1.count(ASTUtils.find_name(_, newTid.name))
              val Cpost = C_ipost + C_iplusonepost
              val newPostPerm = create.expression(Div, constant(1), constant(Cpost))

              permPatternsIPost(sharedVarTmp)._3.foreach {
                ann => cbFusedParBlock.ensures(replacePerm(newPostPerm, ann))
              }
              permPatternsIPlusOnePost(sharedVarTmp)._3.foreach {
                ann => cbFusedParBlock.ensures(replacePerm(newPostPerm, ann))
              }

            } else if (SV_i(sharedVarTmp).map(s => s.map(g => interpretPermission(g._1)).sum).contains(1)) { // .1.3
              DP.add(sharedVarTmp)
              SV_i(sharedVarTmp).flatMap(s => s.map(_._2)).distinct.foreach(st => cbFusedParBlock.requires(st))


              if (SV_iplusone(sharedVarTmp).map(s => s.map(g => interpretPermission(g._1)).sum).contains(1)) { // .1.3.1
                permPatternsIPlusOnePost(sharedVarTmp)._3.foreach {
                  ann => cbFusedParBlock.ensures(copy_rw.rewrite(ann))
                }

              } else { // 1.3.2
                val C_iplusone = permPatternsIPlusOnePost(sharedVarTmp)._1
                  .count(ASTUtils.find_name(_, newTid.name))
                val newPerm = create.expression(Div, constant(1), constant(C_iplusone))
                permPatternsIPlusOnePost(sharedVarTmp)._3.foreach {
                  ann => cbFusedParBlock.ensures(replacePerm(newPerm, ann))
                }
              }

            } else { // 1.4
              DP.add(sharedVarTmp)
              permPatternsIPre(sharedVarTmp)._3.foreach(cbFusedParBlock.requires)
              val X =
                permPatternsIPre(sharedVarTmp)._1.indices.map { i =>
                  (permPatternsIPre(sharedVarTmp)._1(i),
                    permPatternsIPre(sharedVarTmp)._2(i),
                    permPatternsIPre(sharedVarTmp)._3(i))
                }
                  .filter { case (patt, _, _) => ASTUtils.find_name(patt, newTid.name) }
                  .map {
                    _._2
                  }
                  .reduce(plus)
              val annIPlusOne = conditions
              annIPlusOne.foreach { ann => cbFusedParBlock.requires(replacePerm(minus(constant(1), X), ann)) }

              permPatternsIPlusOnePost(sharedVarTmp)._3.foreach {
                ann => cbFusedParBlock.ensures(copy_rw.rewrite(ann))
              }
            }

          } else { // .2
            patts.indices.foreach {
              i =>
                val pattiplusone = patts(i)
                val permiplusone = perms(i)
                val anniplusone = conditions(i)

                val pattToPermIPost = permPatternsIPost(sharedVarTmp)._1.indices.map { i =>
                  (permPatternsIPost(sharedVarTmp)._1(i),
                    (permPatternsIPost(sharedVarTmp)._2(i),
                    permPatternsIPost(sharedVarTmp)._3(i)))
                }.toMap

                val maxPerm:ASTNode = if (interpretPermission(permiplusone) <= interpretPermission(pattToPermIPost(pattiplusone)._1))
                  pattToPermIPost(pattiplusone)._1
                  else
                  permiplusone

                cbFusedParBlock.requires(replacePerm(maxPerm, pattToPermIPost(pattiplusone)._2))
            }

            permPatternsIPlusOnePost(sharedVarTmp)._3.foreach {
              ann => cbFusedParBlock.ensures(copy_rw.rewrite(ann))
            }
          }
        }



      //1.2
      ASTUtils.conjuncts(contractINonPerm.pre_condition, Star, And, Wrap).forEach { preNonPerm =>
        cbFusedParBlock.requires(rewrite(preNonPerm))
      }

      ASTUtils.conjuncts(contractIPlusOneNonPerm.pre_condition, Star, And, Wrap).forEach { preNonPerm =>
        if (NameScanner.freeVars(preNonPerm).asScala.keySet.filter(!_.equals(newTid.name))
          forall(nonshared.contains)) { // all non-shared
          cbFusedParBlock.requires(rewrite(preNonPerm))
        } else if (DP.forall(dp => !ASTUtils.find_name(preNonPerm, dp))) {
          cbFusedParBlock.requires(rewrite(preNonPerm))
        }
      }

      ASTUtils.conjuncts(contractINonPerm.post_condition, Star, And, Wrap).forEach { postNonPerm =>
        if (NameScanner.freeVars(postNonPerm).asScala.keySet.filter(!_.equals(newTid.name))
          forall(nonshared.contains)) { // all non-shared
          cbFusedParBlock.ensures(rewrite(postNonPerm))
        } else if (DP.forall(dp => !ASTUtils.find_name(postNonPerm, dp))) {
          cbFusedParBlock.ensures(rewrite(postNonPerm))
        }
      }

      ASTUtils.conjuncts(contractIPlusOneNonPerm.post_condition, Star, And, Wrap).forEach { postNonPerm =>
        cbFusedParBlock.ensures(rewrite(postNonPerm))
      }


      ///////////////////
      /// Fuse bodies ///
      ///////////////////
      pbLabel = Some(newLabel)
      val fuseBody = (pr: ParallelRegion) => {
        Seq(pr).map(p => ASTUtils.replace(name(p.blocks.head.iters.head.name), newTid, p).asInstanceOf[ParallelRegion])
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
                newBlock.add(create special(ASTSpecial.Kind.Assert, rewrite(stmt)))
              }
            }
            newBlock
          }
      }

      fuseBody(kernelZeroToI).foreach(_.forEachStmt(l => newParBody.add(copy_rw.rewrite(l))))
      pbLabel = None
      pbNewPerms = create.constant(true)
      fuseBody(kernelIPlusOne).foreach(_.forEachStmt(l => newParBody.add(copy_rw.rewrite(l))))

      ///////////////////////////
      /// Create fused kernel ///
      ///////////////////////////
      val newIter = create.field_decl(newTid.name, create.primitive_type(PrimitiveSort.Integer), rewrite(parBlocks.head.blocks.head.iters.head.init.get))
      val newParBlock = create parallel_block(
        newLabel,
        cbFusedParBlock.getContract(false),
        scala.Array(newIter).toList.asJava,
        newParBody,
        null
      )
      kernelZeroToI = create.region(null, null, newParBlock)
    }

    Output("Dependency Check Method\n\n%s\n\n", depCheckMethod)

    kernelZeroToI
  }

  /**
   *
   * @param preconds The preconditions of a kernel
   * @param shared   All shared variables to consider
   * @param T        The number of thread
   * @return A map from shared variable to a matrix where
   *         1. the first index is the location in the array (of size N)
   *            2. the first index points to a sequence of permissions used in the kernel
   */
  def SV(preconds: ASTNode, shared: Map[String, Int], T: Int, newTid: NameExpression): mutable.Map[String, mutable.Seq[mutable.Seq[(ASTNode, ASTNode)]]] = {
    val res = mutable.Map.empty[String, mutable.Seq[mutable.Seq[(ASTNode, ASTNode)]]]
    var preconditions = preconds
    shared.foreach {
      case (arrayName, size) =>
        res(arrayName) = mutable.Seq.fill(size) {
          mutable.Seq.empty[(ASTNode, ASTNode)]
        }
        preconditions = ASTUtils.replace(create.dereference(name(arrayName), "length"), constant(size), preconditions)
    }


    var conditionLambdas = mutable.Seq.empty[(Int, mutable.Map[String, mutable.Seq[mutable.Seq[(ASTNode, ASTNode)]]]) => Unit]

    ASTUtils.conjuncts(preconditions, Star, And, Wrap).forEach {
      stmt => conditionLambdas = conditionLambdas ++ mutable.Seq(interpretFunction(stmt, newTid))
    }


    for (tid <- 0 until T) {
      conditionLambdas.foreach(
        f => f(tid, res)
      )
      //Apply the lambda functions you have
    }

    res
  }

  def interpretFunction(tree: ASTNode, newTid: NameExpression): (Int, mutable.Map[String, mutable.Seq[mutable.Seq[(ASTNode, ASTNode)]]]) => Unit = tree match {
    case o: OperatorExpression =>
      o.operator match {
        case Implies =>
          // of the form cond(g(tid)) ==> Perm(a[f(tid)], pi).
          val cond = interpretBoolean(o.first, newTid)
          o.second match {
            case o1: OperatorExpression if o1.operator == Perm =>
              val perm = o1.second
              val arrayName = o1.first.asInstanceOf[OperatorExpression].first.asInstanceOf[NameExpression].name // a
              val location = interpretExpression(o1.first.asInstanceOf[OperatorExpression].second, newTid) // f(tid)

              (i, m) => if (cond(i)) m(arrayName)(location(i)) ++= mutable.Seq((perm, tree))
            case _ =>
              Fail("Unsupported operator")
              (i, m) =>
          }
        case Perm =>
          // of the form Perm(a[f(tid)], pi)

          val perm = o.second
          val arrayName = o.first.asInstanceOf[OperatorExpression].first.asInstanceOf[NameExpression].name // a
          val location = interpretExpression(o.first.asInstanceOf[OperatorExpression].second, newTid) // f(tid)

          (i, m) => m(arrayName)(location(i)) ++= mutable.Seq((perm, tree))
      }
    case rest =>
      Fail("Permission precondition does not match the expected format\n%s", rest)
      (i, m) =>
  }

  def interpretExpression(tree: ASTNode, newTid: NameExpression): ((Int) => Int) = tree match {
    case o: OperatorExpression =>
      o.operator match {
        case Mult => (i: Int) => interpretExpression(o.first, newTid)(i) * interpretExpression(o.second, newTid)(i)
        case FloorDiv => (i: Int) => interpretExpression(o.first, newTid)(i) / interpretExpression(o.second, newTid)(i)
        case Plus => (i: Int) => interpretExpression(o.first, newTid)(i) + interpretExpression(o.second, newTid)(i)
        case Minus => (i: Int) => interpretExpression(o.first, newTid)(i) - interpretExpression(o.second, newTid)(i)
        case Mod => (i: Int) => Math.floorMod(interpretExpression(o.first, newTid)(i), interpretExpression(o.second, newTid)(i))
        case _ =>
          Fail("Unsupported operator %s", o.operator)
          (i: Int) => 0
      }
    case c: ConstantExpression if isConstantInteger(c) => (i: Int) => getConstantInteger(c).get
    case n: NameExpression if n.equals(newTid) => (i: Int) => i
  }

  def interpretBoolean(tree: ASTNode, newTid: NameExpression): ((Int) => Boolean) = tree match {
    case o: OperatorExpression =>
      o.operator match {
        case LTE => (i) => interpretExpression(o.first, newTid)(i) <= interpretExpression(o.second, newTid)(i)
        case LT => (i) => interpretExpression(o.first, newTid)(i) < interpretExpression(o.second, newTid)(i)
        case GTE => (i) => interpretExpression(o.first, newTid)(i) >= interpretExpression(o.second, newTid)(i)
        case GT => (i) => interpretExpression(o.first, newTid)(i) > interpretExpression(o.second, newTid)(i)
        case EQ => (i) => interpretExpression(o.first, newTid)(i) == interpretExpression(o.second, newTid)(i)
        case NEQ => (i) => interpretExpression(o.first, newTid)(i) != interpretExpression(o.second, newTid)(i)
        case _ =>
          Fail("Unsupported operator %s", o.operator)
          (i: Int) => false
      }
  }

  /**
   *
   * @param variable The name of an array
   * @param preconds Preconditions of a method
   */
  def findConcreteValue(variable: ASTNode, preconds: ASTNode): Int = {
    ASTUtils.conjuncts(preconds, Star, And, Wrap).forEach {
      case o: OperatorExpression if o.operator == EQ =>
        if ((o.first.isInstanceOf[Dereference] &&
          o.first.asInstanceOf[Dereference].obj.equals(variable) &&
          o.first.asInstanceOf[Dereference].field.equals("length"))
          ||
          o.first.equals(variable)
        ) {
          return getConstantInteger(o.second).getOrElse {
            Fail("The righthand side of the equation (%s) is not a constant", o.second)
            0
          }
        } else if ((o.second.isInstanceOf[Dereference] &&
          o.second.asInstanceOf[Dereference].obj.equals(variable) &&
          o.second.asInstanceOf[Dereference].field.equals("length"))
          ||
          o.second.equals(variable)
        ) {
          return getConstantInteger(o.first).getOrElse {
            Fail("The lefthand side of the equation (%s) is not a constant", o.second)
            0
          }
        }
      case o: OperatorExpression if o.operator == NewArray && o.first.equals(variable) =>
        return getConstantInteger(o.second).getOrElse {
          Fail("The size of the array (%s) is not constant", o.second)
          0
        }
      case _ =>
    }
    Fail("Could not find concrete value for %s", variable)
    0 // Needed to make Scala happy
  }

  def isConstantInteger(node: ASTNode): Boolean = node.isInstanceOf[ConstantExpression] && node.asInstanceOf[ConstantExpression].value.isInstanceOf[IntegerValue]

  def getConstantInteger(node: ASTNode): Option[Int] = {
    if (isConstantInteger(node))
      Some(node.asInstanceOf[ConstantExpression].value.asInstanceOf[IntegerValue].value)
    else
      None
  }


  def getTerms(tree: ASTNode, minus: Boolean = false): Seq[ASTNode] = {
    tree match {
      case o: OperatorExpression if o.operator == Plus => {
        if (!minus)
          getTerms(o.first, minus) ++ getTerms(o.second, minus)
        else
          getTerms(create.expression(UMinus, o.first), !minus) ++ getTerms(create.expression(UMinus, o.second), !minus)
      }
      case o: OperatorExpression if o.operator == Minus => {
        getTerms(o.first, minus) ++ getTerms(o.second, !minus)
      }
      case o: OperatorExpression if o.operator == UMinus =>
        getTerms(o.first, !minus)
      case o: OperatorExpression if o.operator == Wrap => getTerms(o.first, minus)
      case _ =>
        if (minus)
          Seq(create.expression(UMinus, tree))
        //ensures Perm( b [ vct_fused_tid ] , 1 \ 2 + (1 \ 1 - 1 \ 2) - (1 \ 2 + (1 - 1 \ 2) - (1 \ 2 + (1 - 1 \ 2) - (0 \ 1 + (1 - 1 \ 2))) + 0 \ 1) );


        else
          Seq(tree)
    }

  }

  var pbLabel: Option[String] = None
  var pbNewPerms: ASTNode = create.constant(true)

  override def visit(pb: ParallelBarrier): Unit = {
    val cbPB = new ContractBuilder
    rewrite(pb.contract, cbPB)
    if (!pbNewPerms.equals(create.constant(true))) {
      cbPB.requires(rewrite(pbNewPerms))
      cbPB.ensures(rewrite(pbNewPerms))
    }
    result = create.barrier(pbLabel.getOrElse(pb.label), cbPB.getContract(), pb.invs, rewrite(pb.body))
  }

  def replacePerm(newPerm: ASTNode, tree: ASTNode): ASTNode = {
    val rw = new AbstractRewriter(null.asInstanceOf[ProgramUnit]) {
      override def visit(e: OperatorExpression): Unit = {
        if (e.operator == Perm)
          result = create.expression(Perm, rewrite(e.first), rewrite(newPerm))
        else super.visit(e)
      }
    }
    rw.rewrite(tree)
  }

  override def visit(e: OperatorExpression): Unit = {
    e.operator match {
      case Perm =>
        val permission = e.second match {
          case ConstantExpression(value)
            if value.isInstanceOf[IntegerValue] && value.asInstanceOf[IntegerValue].value == 1 =>
            create.expression(Div, constant(1), constant(1))
          case NameExpression(name, reserved, kind) if reserved == ASTReserved.FullPerm =>
            create.expression(Div, constant(1), constant(1))
          case o: OperatorExpression if o.operator == Plus =>
            val fst = o.first match {
              case ConstantExpression(value)
                if value.isInstanceOf[IntegerValue] && value.asInstanceOf[IntegerValue].value == 1 =>
                create.expression(Div, constant(1), constant(1))
              case NameExpression(name, reserved, kind) if reserved == ASTReserved.FullPerm =>
                create.expression(Div, constant(1), constant(1))
              case _ => rewrite(o.first)
            }
            val snd = o.second match {
              case ConstantExpression(value)
                if value.isInstanceOf[IntegerValue] && value.asInstanceOf[IntegerValue].value == 1 =>
                create.expression(Div, constant(1), constant(1))
              case NameExpression(name, reserved, kind) if reserved == ASTReserved.FullPerm =>
                create.expression(Div, constant(1), constant(1))
              case _ => rewrite(o.second)
            }
            plus(fst, snd)
          case o: OperatorExpression if o.operator == Minus =>
            val fst = o.first match {
              case ConstantExpression(value)
                if value.isInstanceOf[IntegerValue] && value.asInstanceOf[IntegerValue].value == 1 =>
                create.expression(Div, constant(1), constant(1))
              case NameExpression(name, reserved, kind) if reserved == ASTReserved.FullPerm =>
                create.expression(Div, constant(1), constant(1))
              case _ => rewrite(o.first)
            }
            val snd = o.second match {
              case ConstantExpression(value)
                if value.isInstanceOf[IntegerValue] && value.asInstanceOf[IntegerValue].value == 1 =>
                create.expression(Div, constant(1), constant(1))
              case NameExpression(name, reserved, kind) if reserved == ASTReserved.FullPerm =>
                create.expression(Div, constant(1), constant(1))
              case _ => rewrite(o.second)
            }
            minus(fst, snd)
          case _ => rewrite(e.second)
        }
        result = create.expression(Perm, rewrite(e.first), permission)
      case _ => super.visit(e)
    }
  }

  def interpretPermission(perm: ASTNode): scala.Float = {
    perm match {
      case n: NameExpression if n.isReserved(ASTReserved.FullPerm) => 1
      case n: NameExpression if n.isReserved(ASTReserved.NoPerm) => 0
      case o: OperatorExpression if o.operator == Div => interpretPermission(o.first) / interpretPermission(o.second)
      case o: OperatorExpression if o.operator == Plus => interpretPermission(o.first) + interpretPermission(o.second)
      case o: OperatorExpression if o.operator == Minus => interpretPermission(o.first) - interpretPermission(o.second)
      case o: OperatorExpression if o.operator == UMinus => -1 * interpretPermission(o.second)
      case o: OperatorExpression if o.operator == Wrap => interpretPermission(o.first)
      case c: ConstantExpression if c.value.isInstanceOf[IntegerValue] => c.value.asInstanceOf[IntegerValue].value
      case anyelse =>
        Fail("Could not interpret %s as a concrete permission.", anyelse)
        return -1
    }
  }

  //  def isWritePerm(perm: ASTNode): Boolean = {
  //      perm match {
  //        case ConstantExpression(value) => value.asInstanceOf[IntegerValue].value == 1
  //        case as: ASTReserved => as == ASTReserved.FullPerm
  //        case o: OperatorExpression if o.operator == Div => o.first.equals(constant(1)) && o.first.equals(o.second)
  //        case _ => false
  //      }
  //  }
  //
  //  def isReadPerm(perm: ASTNode): Boolean = !isWritePerm(perm)

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

  case class WriteOrRead(vars: Set[String]) extends RecursiveVisitor(null: ProgramUnit) {

    var varToWritten = mutable.Map.empty[String, Boolean]

    override def visit(s: AssignmentStatement): Unit = {
      s.location match {
        case o: OperatorExpression if o.operator == Subscript =>
          o.first match {
            case name: NameExpression if vars.contains(name.name) =>
              varToWritten(name.name) = true
            case _ =>
          }
          o.second.accept(this)
        case _ =>
          super.visit(s)
      }
    }

    override def visit(e: OperatorExpression): Unit = {
      e.operator match {
        case Subscript =>
          e.first match {
            case name: NameExpression if vars.contains(name.name) =>
              if (!varToWritten.contains(name.name)) {
                varToWritten(name.name) = false
              }
              e.second.accept(this)
            case _ =>
              super.visit(e)
          }
        case _ => super.visit(e)
      }
      super.visit(e)
    }
  }

}
