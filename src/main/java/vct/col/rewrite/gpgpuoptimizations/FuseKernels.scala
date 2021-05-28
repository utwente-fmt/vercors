package vct.col.rewrite.gpgpuoptimizations

import hre.ast.MessageOrigin
import hre.lang.System.Output
import vct.col.ast.`type`.{ASTReserved, PrimitiveSort}
import vct.col.ast.`type`.PrimitiveSort._
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.expr.{NameExpression, OperatorExpression}
import vct.col.ast.expr.constant.{ConstantExpression, IntegerValue}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, ParallelBlock, ParallelRegion}
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
    val block = create.block()
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
                  val parblock = fuseParBlocks(parBlocks)
                  block.add(create region(null, null, parblock))
                }
              } else {
                block.add(copy_rw.rewrite(pr))
              }
            }
            case stmt => {
              if (nf != 0) {
                Fail("No statements allowed between parallel regions while fusion", stmt.getOrigin)
              }
              //TODO OS on which statements should it fail and which not?
              block.add(rewrite(stmt))
            }
          }
    }
    if (nf != 0) {
//      Fail("There are not enough kernels to fuse as specified in one of the gpuopt annotations")
    }
    result = block

  }

  private def fuseParBlocks(parBlocks: Seq[ParallelRegion]) = {
    val parreg = parBlocks.head
    val tid = parreg.blocks.head.iters.head.name
    val parbody = create.block()
    //TODO OS check whether each region only has one block
    //TODO OS check whether each one block only had one iter

    // check if all kernels to fuse have the same configuration
    val distinctBounds = parBlocks.map(_.blocks.head.iters.head.init.get.asInstanceOf[OperatorExpression])
      .map(n => (n.first, n.second))
      .distinct

    if (distinctBounds.size > 1) {
      Fail("All kernels (and parallel blocks) to fuse must have the same thread configuration")
    }

    val newTid = name("vct_fused_tid")

    /////////////////////////////////////
    /// Which variables are shared??? ///
    /////////////////////////////////////
    var shared = mutable.Set.empty[String]
    var nonshared = mutable.Set.empty[String]
    for (pb <- parBlocks) {
      shared = shared.intersect(NameScanner.freeVars(pb).keySet().asScala)
    }

      /////////////////////////////
    /// Data dependency check ///
    /////////////////////////////
    val dataDepCheckMethod = generateDepCheck(parBlocks, newTid)
    methodsWithFusion(current_method().name) = methodsWithFusion(current_method().name) ++ mutable.Buffer(dataDepCheckMethod)

    ////////////
    /// Fuse ///
    ////////////
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
        if (pbi._2 < parBlocks.length-1) {
          ASTUtils.conjuncts(pbi._1.blocks.head.contract.post_condition, Star, And, Wrap).forEach { stmt =>
            newBlock.add(create special(ASTSpecial.Kind.Assert, copy_rw.rewrite(stmt)))
          }
        }
        newBlock
      }
    parblocks.foreach(_.forEachStmt(l => parbody.add(rewrite(l))))

    /*
              for each (consecutive) kernels i and i+1:
                    1. Precondition:
                         [DONE] Permissions: Taken from the dependency check method contract
                         Functional correctness:
                            Take all preconditions for non-shared variables
                            Take the preconditions of the first kernel for shared variable

                    2. Postcondition:
                          Permissions:
                            Take all postconditions for non-shared variables
                            Take the postconditions of the last kernel for shared variable
                          Functional:
                            Take all postconditions for non-shared variables
                            Take the postconditions of the last kernel for shared variable

                    3. [DONE] Body:
                        [DONE] Take the body of kernel i.
                        [DONE] Append the postconditions of kernel i as assertions.
                        [DONE] If kernel i+1 is the last kernel, append the body of kernel i + 1.
            */

    val cbParBlock = new ContractBuilder()
    cbParBlock.requires(copy_rw.rewrite(dataDepCheckMethod.getContract().pre_condition))


    for (i <- 0 to parBlocks.size - 2) {
      val prunedcbi = new ContractBuilder()

      ASTUtils.conjuncts(parBlocks(i).blocks.head.contract.pre_condition, Star, And, Wrap).forEach { stmt =>
        val cp = new CollectPerms()
        cp.rewrite(stmt)
        if (cp.perm.isEmpty) {
          prunedcbi.requires(stmt)
        }
      }

      val prunedcbiplusone = new ContractBuilder()
      ASTUtils.conjuncts(parBlocks(i + 1).blocks.head.contract.pre_condition, Star, And, Wrap).forEach { stmt =>
        val cp = new CollectPerms()
        cp.rewrite(stmt)
        if (cp.perm.isEmpty) {
          prunedcbiplusone.requires(stmt)
        }
      }

      val freeVarsI = NameScanner.freeVars(prunedcbi.getContract().pre_condition).asScala
        .filter(kv => SequenceUtils.getTypeInfo(kv._2) != null && SequenceUtils.getTypeInfo(kv._2).getSequenceSort.equals(Array))
      val freeVarsIplusone = NameScanner.freeVars(prunedcbiplusone.getContract().pre_condition).asScala
        .filter(kv => SequenceUtils.getTypeInfo(kv._2) != null && SequenceUtils.getTypeInfo(kv._2).getSequenceSort.equals(Array))

      val prunedContractI =
        ASTUtils.replace(name(parBlocks(i).blocks.head.iters.head.name), newTid, prunedcbi.getContract()).asInstanceOf[Contract]
      val prunedContractIPlusOne =
        ASTUtils.replace(name(parBlocks(i + 1).blocks.head.iters.head.name), newTid, prunedcbiplusone.getContract()).asInstanceOf[Contract]


      val varsI = freeVarsI.keySet
      val varsIplusone = freeVarsIplusone.keySet

      val shared = varsI.intersect(varsIplusone)
      val nonshared = (varsI diff varsIplusone) union (varsIplusone diff varsI)



    }











//    for (i <- 0 to parBlocks.size - 2) {
//      val prunedcbi = new ContractBuilder()
//
//      ASTUtils.conjuncts(parBlocks(i).blocks.head.contract.pre_condition, Star, And, Wrap).forEach { stmt =>
//        val cp = new CollectPerms()
//        cp.rewrite(stmt)
//        if (cp.perm.isDefined) {
//          prunedcbi.requires(stmt)
//        }
//      }
//      ASTUtils.conjuncts(parBlocks(i).blocks.head.contract.post_condition, Star, And, Wrap).forEach { stmt =>
//        val cp = new CollectPerms()
//        cp.rewrite(stmt)
//        if (cp.perm.isDefined) {
//          prunedcbi.ensures(stmt)
//        }
//      }
//      val prunedcbiplusone = new ContractBuilder()
//      ASTUtils.conjuncts(parBlocks(i + 1).blocks.head.contract.pre_condition, Star, And, Wrap).forEach { stmt =>
//        val cp = new CollectPerms()
//        cp.rewrite(stmt)
//        if (cp.perm.isDefined) {
//          prunedcbiplusone.requires(stmt)
//        }
//      }
//
//      val freeVarsI = NameScanner.freeVars(prunedcbi.getContract().pre_condition).asScala
//        .filter(kv => SequenceUtils.getTypeInfo(kv._2) != null && SequenceUtils.getTypeInfo(kv._2).getSequenceSort.equals(Array))
//      val freeVarsIplusone = NameScanner.freeVars(prunedcbiplusone.getContract().pre_condition).asScala
//        .filter(kv => SequenceUtils.getTypeInfo(kv._2) != null && SequenceUtils.getTypeInfo(kv._2).getSequenceSort.equals(Array))
//
//      val prunedContractI =
//        ASTUtils.replace(name(parBlocks(i).blocks.head.iters.head.name), newTid, prunedcbi.getContract()).asInstanceOf[Contract]
//      val prunedContractIPlusOne =
//        ASTUtils.replace(name(parBlocks(i + 1).blocks.head.iters.head.name), newTid, prunedcbiplusone.getContract()).asInstanceOf[Contract]
//
//
//      val varsI = freeVarsI.keySet
//      val varsIplusone = freeVarsIplusone.keySet
//
//      val shared = varsI.intersect(varsIplusone)
//      val nonshared = (varsI diff varsIplusone) union (varsIplusone diff varsI)
//
//      nonshared.foreach { nonsharedVar =>
//        var foundAny = false
//        ASTUtils.conjuncts(prunedContractI.pre_condition, Star, And, Wrap).forEach { stmt =>
//          val found = ASTUtils.find_name(stmt, nonsharedVar)
//          if (found) {
//            cbDepCheck.requires(rewrite(stmt))
//            foundAny = true
//          }
//        }
//        if (!foundAny) {
//          ASTUtils.conjuncts(prunedContractIPlusOne.pre_condition, Star, And, Wrap).forEach { stmt =>
//            if (ASTUtils.find_name(stmt, nonsharedVar)) {
//              cbDepCheck.requires(rewrite(stmt))
//            }
//          }
//        }
//      }
//
//      shared.foreach { sharedVar =>
//        ASTUtils.conjuncts(prunedContractI.pre_condition, Star, And, Wrap).forEach { stmt =>
//          if (ASTUtils.find_name(stmt, sharedVar)) {
//            cbDepCheck.requires(rewrite(stmt))
//          }
//        }
//      }
//
//
//      val preiPlusOne = ASTUtils.conjuncts(prunedContractIPlusOne.pre_condition, Star, And, Wrap).asScala
//        .filter(pre => shared.exists(ASTUtils.find_name(pre, _)))
//        .map { pre =>
//          val cp = new CollectPerms()
//          cp.rewrite(pre)
//          val res = cp.perm.get
//          (rewrite(res.first), (rewrite(res.second), rewrite(pre)))
//        }
//        .groupBy(_._1)
//        .map(kv => (kv._1, kv._2.map(kv2 => kv2._2)))
//        .map { kv =>
//          val value = kv._2.foldLeft((Seq.empty[ASTNode], Seq.empty[ASTNode])) {
//            (prev, next) =>
//              (prev._1 :+ next._1, prev._2 :+ next._2)
//          }
//          (kv._1, value)
//        }
//
//      val posti = ASTUtils.conjuncts(prunedContractI.post_condition, Star, And, Wrap).asScala
//        .filter(pre => shared.exists(ASTUtils.find_name(pre, _)))
//        .map { pre =>
//          val cp = new CollectPerms()
//          cp.rewrite(pre)
//          val res = cp.perm.get
//          (rewrite(res.first), (rewrite(res.second), rewrite(pre)))
//        }
//        .groupBy(_._1)
//        .map(kv => (kv._1, kv._2.map(kv2 => kv2._2)))
//        .map { kv =>
//          val value = kv._2.foldLeft((Seq.empty[ASTNode], Seq.empty[ASTNode])) {
//            (prev, next) =>
//              (prev._1 :+ next._1, prev._2 :+ next._2)
//          }
//          (kv._1, value)
//        }
//
//      /*
//                for each (consecutive) kernels i and i+1:
//                    0. [DONE] ignore out all non-permission related contracts
//                    1. [DONE] Replace all different thread identifiers in the separate kernels by a unique new thread identifier
//                    2. [DONE] distinguish variables shared between the parblock contracts.
//                    3. [DONE] Create an empty contract.
//                    4. [DONE] for non-shared variables, add the precondition statements of both parblock contracts to
//                        the new contract
//                    5. [DONE] Add all preconditions of kernel i related to any shared variable
//
//                    6. [DONE] Construct a map from the patterns to a list of permissions (a[tid] -> ([1/4, 1/2], [pre1, pre2]))
//                        for the postcondition of kernel i and the precondition of kernel i+1
//
//                    7. for each pattern e1 using a shared variable a in precondition PermPre(a[e1]) of kernel i+1,
//                          search for the permission pattern e1 in the postcondition of kernel i,
//                    7.1 [DONE] If the permission pattern e1 is not found,
//                          add the precondition PermPre(a[e1]) in kernel i+1 to the new contract
//                    7.2 [DONE] Else if, the permission fraction for a[e1] is the same as in the postcondition of kernel i,
//                          then continue (the permission is already added in 2.)
//                    7.3 [DONE] Else if the permission fraction for a[e1] is smaller than the permission in the postcondition of kernel i,
//                          then continue (the permission is already added in 2.)
//
//                    7.4 Else if the permission fraction for a[e1] is larger than the permission in the postcondition of kernel i,
//                          then add a separate permission predicate (Perm) for a[e1] with the difference (in permission) between kernel i+1 and i
//                    // Only for merging, add the new permission predicate to the contract of existing barriers in kernel i
//              */
//
//      preiPlusOne.foreach { kv =>
//        if (!posti.contains(kv._1)) {
//          kv._2._2.foreach(s => cbDepCheck.requires(rewrite(s))) // Do 7.1
//        } else if (kv._2._1.map(interpretPermission).sum > posti(kv._1)._1.map(interpretPermission).sum) {
//          // Do 7.4
//          val additionalPerm = rewrite(minus(
//            kv._2._1.reduce((lhs, rhs) => plus(rewrite(lhs), rewrite(rhs))),
//            posti(kv._1)._1.reduce((lhs, rhs) => plus(rewrite(lhs), rewrite(rhs)))
//          )
//          )
//          val newRequires = create.expression(Perm, rewrite(kv._1), additionalPerm)
//          cbDepCheck.requires(newRequires)
//        }
//        // else if (posti(kv._1)._1 == kv._2._1) { // 7.2 Do nothing }
//        // else if (kv._2._1 < posti(kv._1)._1 ) { // 7.3Do nothing }
//      }
//
//    }
























    val newIter = create.field_decl(newTid.name, create.primitive_type(PrimitiveSort.Integer), rewrite(parBlocks.head.blocks.head.iters.head.init.get))
    val parblock = create parallel_block(
      "vct_fused_kernels",
      cbParBlock.getContract(false),
      scala.Array(newIter).toList.asJava,
      parbody,
      null
    )
    parblock
  }

  private def generateDepCheck(parBlocks: Seq[ParallelRegion], newTid: NameExpression): Method = {
    val cbDepCheck = new ContractBuilder()
    var i = 0
    var shared = mutable.Set.empty[String]
    var nonshared = mutable.Set.empty[String]
    // For each parallel region
    for (i <- 0 to parBlocks.size - 2) {
      val prunedcbi = new ContractBuilder()

      ASTUtils.conjuncts(parBlocks(i).blocks.head.contract.pre_condition, Star, And, Wrap).forEach { stmt =>
        val cp = new CollectPerms()
        cp.rewrite(stmt)
        if (cp.perm.isDefined) {
          prunedcbi.requires(stmt)
        }
      }
      ASTUtils.conjuncts(parBlocks(i).blocks.head.contract.post_condition, Star, And, Wrap).forEach { stmt =>
        val cp = new CollectPerms()
        cp.rewrite(stmt)
        if (cp.perm.isDefined) {
          prunedcbi.ensures(stmt)
        }
      }
      val prunedcbiplusone = new ContractBuilder()
      ASTUtils.conjuncts(parBlocks(i + 1).blocks.head.contract.pre_condition, Star, And, Wrap).forEach { stmt =>
        val cp = new CollectPerms()
        cp.rewrite(stmt)
        if (cp.perm.isDefined) {
          prunedcbiplusone.requires(stmt)
        }
      }

      val prunedContractI =
        ASTUtils.replace(name(parBlocks(i).blocks.head.iters.head.name), newTid, prunedcbi.getContract()).asInstanceOf[Contract]
      val prunedContractIPlusOne =
        ASTUtils.replace(name(parBlocks(i + 1).blocks.head.iters.head.name), newTid, prunedcbiplusone.getContract()).asInstanceOf[Contract]

      val freeVarsI = NameScanner.freeVars(prunedcbi.getContract().pre_condition).asScala
        .filter(kv => SequenceUtils.getTypeInfo(kv._2) != null && SequenceUtils.getTypeInfo(kv._2).getSequenceSort.equals(Array))
      val freeVarsIplusone = NameScanner.freeVars(prunedcbiplusone.getContract().pre_condition).asScala
        .filter(kv => SequenceUtils.getTypeInfo(kv._2) != null && SequenceUtils.getTypeInfo(kv._2).getSequenceSort.equals(Array))

      val varsI = freeVarsI.keySet
      val varsIplusone = freeVarsIplusone.keySet

      shared = shared union varsI.intersect(varsIplusone)
      nonshared = nonshared union ((varsI diff varsIplusone) union (varsIplusone diff varsI))


      nonshared.foreach { nonsharedVar =>
        var foundAny = false
        ASTUtils.conjuncts(prunedContractI.pre_condition, Star, And, Wrap).forEach { stmt =>
          val found = ASTUtils.find_name(stmt, nonsharedVar)
          if (found) {
            cbDepCheck.requires(rewrite(stmt))
            foundAny = true
          }
        }
        if (!foundAny) {
          ASTUtils.conjuncts(prunedContractIPlusOne.pre_condition, Star, And, Wrap).forEach { stmt =>
            if (ASTUtils.find_name(stmt, nonsharedVar)) {
              cbDepCheck.requires(rewrite(stmt))
            }
          }
        }
      }

      shared.foreach { sharedVar =>
        ASTUtils.conjuncts(prunedContractI.pre_condition, Star, And, Wrap).forEach { stmt =>
          if (ASTUtils.find_name(stmt, sharedVar)) {
            cbDepCheck.requires(rewrite(stmt))
          }
        }
      }


      val preiPlusOne = ASTUtils.conjuncts(prunedContractIPlusOne.pre_condition, Star, And, Wrap).asScala
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

      val posti = ASTUtils.conjuncts(prunedContractI.post_condition, Star, And, Wrap).asScala
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

      /*
                for each (consecutive) kernels i and i+1:
                    0. [DONE] ignore out all non-permission related contracts
                    1. [DONE] Replace all different thread identifiers in the separate kernels by a unique new thread identifier
                    2. [DONE] distinguish variables shared between the parblock contracts.
                    3. [DONE] Create an empty contract.
                    4. [DONE] for non-shared variables, add the precondition statements of both parblock contracts to
                        the new contract
                    5. [DONE] Add all preconditions of kernel i related to any shared variable

                    6. [DONE] Construct a map from the patterns to a list of permissions (a[tid] -> ([1/4, 1/2], [pre1, pre2]))
                        for the postcondition of kernel i and the precondition of kernel i+1

                    7. for each pattern e1 using a shared variable a in precondition PermPre(a[e1]) of kernel i+1,
                          search for the permission pattern e1 in the postcondition of kernel i,
                    7.1 [DONE] If the permission pattern e1 is not found,
                          add the precondition PermPre(a[e1]) in kernel i+1 to the new contract
                    7.2 [DONE] Else if, the permission fraction for a[e1] is the same as in the postcondition of kernel i,
                          then continue (the permission is already added in 2.)
                    7.3 [DONE] Else if the permission fraction for a[e1] is smaller than the permission in the postcondition of kernel i,
                          then continue (the permission is already added in 2.)

                    7.4 Else if the permission fraction for a[e1] is larger than the permission in the postcondition of kernel i,
                          then add a separate permission predicate (Perm) for a[e1] with the difference (in permission) between kernel i+1 and i
                    // Only for merging, add the new permission predicate to the contract of existing barriers in kernel i
              */

      preiPlusOne.foreach { kv =>
        if (!posti.contains(kv._1)) {
          kv._2._2.foreach(s => cbDepCheck.requires(rewrite(s))) // Do 7.1
        } else if (kv._2._1.map(interpretPermission).sum > posti(kv._1)._1.map(interpretPermission).sum) {
          // Do 7.4
          val additionalPerm = rewrite(minus(
            kv._2._1.reduce((lhs, rhs) => plus(rewrite(lhs), rewrite(rhs))),
            posti(kv._1)._1.reduce((lhs, rhs) => plus(rewrite(lhs), rewrite(rhs)))
          )
          )
          val newRequires = create.expression(Perm, rewrite(kv._1), additionalPerm)
          cbDepCheck.requires(newRequires)
        }
        // else if (posti(kv._1)._1 == kv._2._1) { // 7.2 Do nothing }
        // else if (kv._2._1 < posti(kv._1)._1 ) { // 7.3Do nothing }
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
    depCheckMethod
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
