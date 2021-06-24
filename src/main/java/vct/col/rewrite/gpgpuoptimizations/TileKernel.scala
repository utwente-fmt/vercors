package vct.col.rewrite.gpgpuoptimizations

import java.util

import vct.col.ast.`type`.PrimitiveSort
import vct.col.ast.expr.{Binder, BindingExpression, MethodInvokation, NameExpression, OperatorExpression, StandardOperator}
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.expr.constant.{ConstantExpression, IntegerValue}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, ParallelInvariant, ParallelRegion}
import vct.col.ast.stmt.decl.TilingConfig._
import vct.col.ast.stmt.decl.{ASTSpecial, DeclarationStatement, Method, ProgramUnit, Tiling}
import vct.col.ast.util.{ASTUtils, AbstractRewriter, ContractBuilder, NameScanner, RecursiveVisitor}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.Seq

case class TileKernel(override val source: ProgramUnit) extends AbstractRewriter(source) {

  case class FindOlds(override val source: ProgramUnit, var olds: mutable.Set[OperatorExpression] = mutable.Set.empty[OperatorExpression]) extends RecursiveVisitor[ASTNode](source) {
    override def visit(e: OperatorExpression): Unit = e.operator match {
      case Old =>
        olds = olds + e
      case _ => super.visit(e)
    }
  }


  val newIdxFuncName = "vct_tile_newIdx"
  val lowFuncName = "vct_tile_low"
  val uppFuncName = "vct_tile_upp"
  val ceilingFuncName = "vct_tile_ceiling"

  var mapTidto: Option[(NameExpression, ASTNode)] = None

  def addCeilingFunc(): Unit = {
    if (currentTargetClass.find_predicate(ceilingFuncName) != null) {
      return
    }
    val cb = new ContractBuilder
    cb.requires(gt(create.local_name("b"), constant(0)))
    val func = create.function_decl(
      create.primitive_type(PrimitiveSort.Integer),
      cb.getContract(true),
      ceilingFuncName,
      Seq(
        new DeclarationStatement("a", create.primitive_type(PrimitiveSort.Integer)),
        new DeclarationStatement("b", create.primitive_type(PrimitiveSort.Integer)),
      ).asJava,
      create.expression(ITE,
        eq(mod(create.local_name("a"), create.local_name("b")), constant(0)),
        floordiv(create.local_name("a"), create.local_name("b")),
        plus(floordiv(create.local_name("a"), create.local_name("b")), constant(1)),
      )
    )
    currentTargetClass.add_static(func)
  }

  def addNewIdxFunc(): Unit = {
    if (currentTargetClass.find_predicate(newIdxFuncName) != null) {
      return
    }
    val func = create.function_decl(
      create.primitive_type(PrimitiveSort.Integer),
      null,
      newIdxFuncName,
      Seq(
        new DeclarationStatement("a", create.primitive_type(PrimitiveSort.Integer)),
        new DeclarationStatement("b", create.primitive_type(PrimitiveSort.Integer)),
        new DeclarationStatement("c", create.primitive_type(PrimitiveSort.Integer)),
      ).asJava,
      create.expression(Plus,
        create.local_name("a"),
        create.expression(Mult, create.local_name("b"), create.local_name("c"))
      )
    )
    currentTargetClass.add_static(func)
  }

  def addLowerAndUppFunc(): Unit = {
    if (currentTargetClass.find_predicate(lowFuncName) == null) {
      val lowerfunc = create.function_decl(
        create.primitive_type(PrimitiveSort.Integer),
        null,
        lowFuncName,
        Seq(
          new DeclarationStatement("a", create.primitive_type(PrimitiveSort.Integer)),
          new DeclarationStatement("b", create.primitive_type(PrimitiveSort.Integer)),
        ).asJava,
        create.expression(Mult, create.local_name("a"), create.local_name("b")
        )
      )
      currentTargetClass.add_static(lowerfunc)
    }
    if (currentTargetClass.find_predicate(uppFuncName) == null) {
      val upperfunc = create.function_decl(
        create.primitive_type(PrimitiveSort.Integer),
        null,
        uppFuncName,
        Seq(
          new DeclarationStatement("a", create.primitive_type(PrimitiveSort.Integer)),
          new DeclarationStatement("b", create.primitive_type(PrimitiveSort.Integer)),
        ).asJava,
        create.expression(Mult, plus(create.local_name("a"), constant(1)), create.local_name("b")
        )
      )
      currentTargetClass.add_static(upperfunc)
    }

  }

  override def visit(m: Method): Unit = {
    val opts = m.getGpuOpts.asScala.filter(_.isInstanceOf[Tiling]).toList
    if (opts.isEmpty) {
      super.visit(m)
      return
    }
    val opt = opts.head.asInstanceOf[Tiling]

    if (opt.tileSizeInt <= 1) {
      Warning("Tiling is not performed. The tile size is less than two.")
      super.visit(m)
      return
    }

    //TODO OS match on parallelregion instead of method
    result = opt.interOrIntra match {
      case Intra => intraTiling(m, opt)
      case Inter => interTiling(m, opt)
    }

    addCeilingFunc()
  }

  def intraTiling(m: Method, opt: Tiling): Method = {
    //TODO OS write all the ifs written informally


    //    check m.getBody.isInstanceOf[BlockStatement]
    //    check m.getBody.asInstanceOf[BlockStatement].getStatement(0).isInstanceOf[ParallelRegion]
    val region = m.getBody.asInstanceOf[BlockStatement].getStatement(0) match {
      case pb: ParallelRegion => pb
      case ParallelInvariant(label, inv, block) => block.getStatement(0).asInstanceOf[ParallelRegion]
    }
//    val region = m.getBody.asInstanceOf[BlockStatement].getStatement(0).asInstanceOf[ParallelRegion]
    //    check region.blocks.size == 1
    val parBlock = region.blocks.head
    //    check parBlock.iters.size == 1
    val tidDecl = parBlock.iters.head
    val tid = tidDecl.name
    //    check tidDecl.init.isInstanceOf[OperatorExpression]
    //    check tidDecl.init.asInstanceOf[OperatorExpression].op == RangeSeq
    val upperBoundNode = tidDecl.init.get.asInstanceOf[OperatorExpression].second

    val upperBound = findUpperbound(m, opt, tid, upperBoundNode)

    if (upperBound <= opt.tileSizeInt) {
      Fail("The tile size %s must be less than the upperbound %s of %s", opt.tileSize, constant(upperBound), tid)
    }

    addLowerAndUppFunc()

    ////////////////
    ////////////////
    ////////////////
    ////////////////

    val parBody = parBlock.block
    val parContract = parBlock.contract
    val parTid = parBlock.iters.head
    val parLabel = parBlock.label
    val forallVarName = "vct_i"

    val itervar = "vct_tile_counter"

    mapTidto = Some((create.unresolved_name(tid), create.local_name(forallVarName)))

    val cb = new ContractBuilder
    val cbLoop = new ContractBuilder
    ASTUtils.conjuncts(parContract.pre_condition, And, Star).forEach(pre =>
      if (NameScanner.accesses(pre).contains(tid)) {
        cb.requires(tileOnNodeInter(opt, tid, upperBoundNode, forallVarName, pre))
        if (pre.getType != null && pre.getType.isPrimitive(PrimitiveSort.Resource)) {
          cbLoop.appendInvariant(tileOnNodeInter(opt, tid, upperBoundNode, forallVarName, pre))
        } else {
          cbLoop.appendInvariant(tileOnNodeInterWithLow(opt, tid, upperBoundNode, forallVarName, pre, name(itervar)))
        }
      } else {
        cb.requires(copy_rw.rewrite(pre))
        if (pre.getType != null && pre.getType.isPrimitive(PrimitiveSort.Resource)) {
          cb.appendInvariant(copy_rw.rewrite(pre))
        }
      }
    )

    ASTUtils.conjuncts(parContract.post_condition, And, Star).forEach(post => {
      if (NameScanner.accesses(post).contains(tid)) {
        val findOlds = FindOlds(null)
        post.accept(findOlds)
        val olds = findOlds.olds
        olds.foreach(old => {
          val newPost = eq(copy_rw.rewrite(old), old.first)
          cbLoop.appendInvariant(
            tileOnNodeInterWithLow(opt, tid, upperBoundNode, forallVarName, newPost, name(itervar))
          )
        })
      }

      if (NameScanner.accesses(post).contains(tid)) {
        cb.ensures(tileOnNodeInter(opt, tid, upperBoundNode, forallVarName, post))
        if (post.getType != null && !post.getType.isPrimitive(PrimitiveSort.Resource)) {
          cbLoop.appendInvariant(tileOnNodeInterWithUppWithoutAdd(opt, tid, upperBoundNode, forallVarName, post, name(itervar)))
        }
      } else {
        cb.ensures(copy_rw.rewrite(post))
      }
    }
    )
    ASTUtils.conjuncts(parContract.invariant, And, Star).forEach(pre =>
      cb.appendInvariant(copy_rw.rewrite(pre))
    )
    ASTUtils.conjuncts(parContract.kernelInvariant, And, Star).forEach(pre =>
      cb.appendKernelInvariant(copy_rw.rewrite(pre))
    )

    cbLoop.prependInvariant(
      and(
        gte(name(itervar), create.invokation(null, null, lowFuncName, create.local_name(tid), opt.tileSize)),
        and(
          lte(name(itervar), create.invokation(null, null, uppFuncName, create.local_name(tid), opt.tileSize)),
          lte(name(itervar), upperBoundNode)
        )
      )
    )

    val newParContract = cb.getContract(true)

    val newParBody = create.block()
    mapTidto = Some((create.unresolved_name(tid), create.local_name(itervar)))

    newParBody.add(create.special(ASTSpecial.Kind.Assert, lte(name(tid), floordiv(upperBoundNode, opt.tileSize))))
    newParBody.add(create.for_loop(
      create.field_decl(itervar, create.primitive_type(PrimitiveSort.Integer), create.invokation(null, null, lowFuncName, create.local_name(tid), opt.tileSize)),
      and(
        less(name(itervar), create.invokation(null, null, uppFuncName, create.local_name(tid), opt.tileSize)),
        less(name(itervar), copy_rw.rewrite(upperBoundNode))
      ),
      create.assignment(name(itervar), plus(name(itervar), constant(1))),
      rewrite(parBody),
      null,
      cbLoop.getContract(false),
    ))


    val newParTid = List(
      DeclarationStatement(
        parTid.name,
        rewrite(parTid.`type`),
        Some(create.expression(RangeSeq, constant(0), invoke(null, ceilingFuncName, upperBoundNode, opt.tileSize))))
    )
    val newParLabel = parBlock.label

    val newParBlock = create.parallel_block(newParLabel, newParContract, newParTid.toArray, newParBody)
    //TODO OS the region contract has to be rewritten according
    //  to the rules for rewriting the method contract.





    val newParRegion = m.getBody.asInstanceOf[BlockStatement].getStatement(0) match {
      case pb: ParallelRegion => create.region(rewrite(pb.fuse), rewrite(region.contract), newParBlock)
      case parinv: ParallelInvariant => {

        var newinvs:ASTNode = create.constant(true)

        ASTUtils.conjuncts(parinv.inv, And, Star).forEach {
          case bindexpr: BindingExpression
            if bindexpr.binder == Binder.Star || bindexpr.binder == Binder.Forall =>

            findBounds(bindexpr.select, name(bindexpr.decls.head.name)) match {
              case Some(bounds) =>
                val j = bindexpr.decls.head.name + "_1"
                val i = "i_0"
                val newSelect = and(
                  and(
                    gte(create.local_name(j), create.invokation(null, null, lowFuncName, create.local_name(i), opt.tileSize)),
                    less(create.local_name(j), create.invokation(null, null, uppFuncName, create.local_name(i), opt.tileSize))
                  ),
                  less(create.local_name(j), bounds._2)
                )

                val newMain = ASTUtils.replace(create.local_name(bindexpr.decls.head.name), create.local_name(j), bindexpr.main)
                val innerForall = create.binder(bindexpr.binder,
                  copy_rw.rewrite(bindexpr.result_type),
                  Array(create.field_decl(j, copy_rw.rewrite(bindexpr.decls.head.`type`))),
                  Array.empty[Array[ASTNode]],
                  newSelect, // ASTNode selection,
                  newMain // ASTNode main
                )


                val outerLoop = create.binder(bindexpr.binder,
                  copy_rw.rewrite(bindexpr.result_type),
                  Array(create.field_decl(i, copy_rw.rewrite(bindexpr.decls.head.`type`))),
                  Array.empty[Array[ASTNode]],
                  and(
                    lte(constant(0), name(i)),
                    less(name(i), invoke(null, ceilingFuncName, bounds._2, opt.tileSize))
                  ),
                  innerForall
                )
                newinvs = star(newinvs, outerLoop)
//                  cbMethod.requires(outerLoop)
              case None => newinvs = star(newinvs, copy_rw.rewrite(bindexpr))
            }
          case pre =>
            newinvs = star(newinvs, copy_rw.rewrite(pre))
        }

        create.invariant_block(parinv.label, newinvs, create.block(create.region(rewrite(region.fuse), rewrite(region.contract), newParBlock)))
      }
      case _ => ???
    }



    val cbMethod = new ContractBuilder()
    cbMethod.appendInvariant(copy_rw.rewrite(m.getContract().invariant))
    cbMethod.given(copy_rw.rewrite(m.getContract().given):_*)
    cbMethod.yields(copy_rw.rewrite(m.getContract().`yields`):_*)
    cbMethod.appendKernelInvariant(copy_rw.rewrite(m.getContract().kernelInvariant))

    ASTUtils.conjuncts(m.getContract().pre_condition, And, Star).forEach {
      case bindexpr: BindingExpression
        if bindexpr.binder == Binder.Star || bindexpr.binder == Binder.Forall =>

        findBounds(bindexpr.select, name(bindexpr.decls.head.name)) match {
          case Some(bounds) =>
            val j = bindexpr.decls.head.name + "_1"
            val i = "i_0"

            val newSelect = and(
              and(
                gte(create.local_name(j), create.invokation(null, null, lowFuncName, create.local_name(i), opt.tileSize)),
                less(create.local_name(j), create.invokation(null, null, uppFuncName, create.local_name(i), opt.tileSize))
              ),
              less(create.local_name(j), bounds._2)
            )

            val newMain = ASTUtils.replace(create.local_name(bindexpr.decls.head.name), create.local_name(j), bindexpr.main)
            val innerForall = create.binder(bindexpr.binder,
              copy_rw.rewrite(bindexpr.result_type),
              Array(create.field_decl(j, copy_rw.rewrite(bindexpr.decls.head.`type`))),
              Array.empty[Array[ASTNode]],
              newSelect, // ASTNode selection,
              newMain // ASTNode main
            )


            val outerLoop = create.binder(bindexpr.binder,
              copy_rw.rewrite(bindexpr.result_type),
              Array(create.field_decl(i, copy_rw.rewrite(bindexpr.decls.head.`type`))),
              Array.empty[Array[ASTNode]],
              and(
                lte(constant(0), name(i)),
                less(name(i), invoke(null, ceilingFuncName, bounds._2, opt.tileSize))
              ),
              innerForall
            )

            cbMethod.requires(outerLoop)
          case None => cbMethod.requires(copy_rw.rewrite(bindexpr))
        }
      case pre =>
        cbMethod.requires(copy_rw.rewrite(pre))
    }

    ASTUtils.conjuncts(m.getContract().post_condition, And, Star).forEach {
      case bindexpr: BindingExpression
        if bindexpr.binder == Binder.Star || bindexpr.binder == Binder.Forall =>

        findBounds(bindexpr.select, name(bindexpr.decls.head.name)) match {
          case Some(bounds) =>
            val j = bindexpr.decls.head.name + "_1"
            val i = "i_0"

            val newSelect = and(
              and(
                gte(create.local_name(j), create.invokation(null, null, lowFuncName, create.local_name(i), opt.tileSize)),
                less(create.local_name(j), create.invokation(null, null, uppFuncName, create.local_name(i), opt.tileSize))
              ),
              less(create.local_name(j), bounds._2)
            )

            val newMain = ASTUtils.replace(create.local_name(bindexpr.decls.head.name), create.local_name(j), bindexpr.main)

            val innerForall = create.binder(bindexpr.binder,
              copy_rw.rewrite(bindexpr.result_type),
              Array(create.field_decl(j, copy_rw.rewrite(bindexpr.decls.head.`type`))),
              Array.empty[Array[ASTNode]],
              newSelect, // ASTNode selection,
              newMain // ASTNode main
            )

            val outerLoop = create.binder(bindexpr.binder,
              copy_rw.rewrite(bindexpr.result_type),
              Array(create.field_decl(i, copy_rw.rewrite(bindexpr.decls.head.`type`))),
              Array.empty[Array[ASTNode]],
              and(
                lte(constant(0), name(i)),
                less(name(i), invoke(null, ceilingFuncName, bounds._2, opt.tileSize))
              ),
              innerForall
            )

            cbMethod.ensures(outerLoop)
          case None => cbMethod.ensures(copy_rw.rewrite(bindexpr))
        }
      case post =>
        cbMethod.ensures(copy_rw.rewrite(post))
    }


    create.method_decl(
      copy_rw.rewrite(m.getReturnType),
      cbMethod.getContract(),
      m.getName,
      copy_rw.rewrite(m.getArgs),
      create.block(newParRegion)
    )
  }

  def interTiling(m: Method, opt: Tiling): Method = {

    //    super.visit(m)
    //    check m.getBody.isInstanceOf[BlockStatement]
    //    check m.getBody.asInstanceOf[BlockStatement].getStatement(0).isInstanceOf[ParallelRegion]
    val region = m.getBody.asInstanceOf[BlockStatement].getStatement(0) match {
      case pb: ParallelRegion => pb
      case ParallelInvariant(label, inv, block) => block.getStatement(0).asInstanceOf[ParallelRegion]
    }    //    check region.blocks.size == 1
    val parBlock = region.blocks.head
    //    check parBlock.iters.size == 1
    val tidDecl = parBlock.iters.head
    val tid = tidDecl.name
    //    check tidDecl.init.isInstanceOf[OperatorExpression]
    //    check tidDecl.init.asInstanceOf[OperatorExpression].op == RangeSeq
    val upperBoundNode = tidDecl.init.get.asInstanceOf[OperatorExpression].second

    val upperBound = findUpperbound(m, opt, tid, upperBoundNode)

    if (upperBound <= opt.tileSizeInt) {
      Fail("The tile size %s must be less than the upperbound %s of %s", opt.tileSize, constant(upperBound), tid)
    }

    addNewIdxFunc()

    ////////////////
    ////////////////
    ////////////////
    ////////////////

    val parBody = parBlock.block
    val parContract = parBlock.contract
    val parTid = parBlock.iters.head
    val parLabel = parBlock.label
    val forallVarName = "vct_i"

    val itervar = "vct_tile_counter"

    mapTidto = Some((create.unresolved_name(tid), create.invokation(null, null, newIdxFuncName, create.local_name(tid), create.local_name(forallVarName), opt.tileSize)))

    val cb = new ContractBuilder
    val cbLoop = new ContractBuilder
    ASTUtils.conjuncts(parContract.pre_condition, And, Star).forEach(pre =>
      if (NameScanner.accesses(pre).contains(tid)) {
        cb.requires(tileOnNode(opt, tid, upperBoundNode, forallVarName, pre))
        if (pre.getType != null && pre.getType.isPrimitive(PrimitiveSort.Resource)) {
          cbLoop.appendInvariant(tileOnNode(opt, tid, upperBoundNode, forallVarName, pre))
        } else {
          cbLoop.appendInvariant(tileOnNodeWithLow(opt, tid, upperBoundNode, forallVarName, pre, name(itervar)))
        }
      } else {
        cb.requires(copy_rw.rewrite(pre))
        if (pre.getType != null && pre.getType.isPrimitive(PrimitiveSort.Resource)) {
          cb.appendInvariant(copy_rw.rewrite(pre))
        }
      }
    )

    ASTUtils.conjuncts(parContract.post_condition, And, Star).forEach(post => {
      if (NameScanner.accesses(post).contains(tid)) {
        val findOlds = FindOlds(null)
        post.accept(findOlds)
        val olds = findOlds.olds
        olds.foreach(old => {
          val newPost = eq(copy_rw.rewrite(old), old.first)
          cbLoop.appendInvariant(
            tileOnNodeWithLow(opt, tid, upperBoundNode, forallVarName, newPost, name(itervar))
          )
        })
      }

      if (NameScanner.accesses(post).contains(tid)) {
        cb.ensures(tileOnNode(opt, tid, upperBoundNode, forallVarName, post))
        if (post.getType != null && !post.getType.isPrimitive(PrimitiveSort.Resource)) {
          cbLoop.appendInvariant(tileOnNodeWithUpper(opt, tid, upperBoundNode, forallVarName, post, name(itervar)))
        }
      } else {
        cb.ensures(copy_rw.rewrite(post))
      }
    }
    )
    ASTUtils.conjuncts(parContract.invariant, And, Star).forEach(pre =>
      cb.appendInvariant(copy_rw.rewrite(pre))
    )
    ASTUtils.conjuncts(parContract.kernelInvariant, And, Star).forEach(pre =>
      cb.appendKernelInvariant(copy_rw.rewrite(pre))
    )
    val newParContract = cb.getContract(true)

    val newParBody = create.block()
    cbLoop.prependInvariant(
      and(
        gte(name(itervar), constant(0)),
        lte(name(itervar), invoke(null, ceilingFuncName, copy_rw.rewrite(upperBoundNode), opt.tileSize))
      )
    )

    /**
     * Pre(i)
     * \forall int i; i >= j && i < ceiling(array.length, N) && arithExp(tid, i, N) < array.length; Pre(i));
     * \^
     * Post(i)
     * \forall int i; i >= 0 && i < j && arithExp(tid, i, N) < array.length; Post(i));
     * \^
     */


    mapTidto = Some((create.unresolved_name(tid), create.invokation(null, null, newIdxFuncName, create.local_name(tid), create.local_name(itervar), opt.tileSize)))

    newParBody.add(create.for_loop(
      create.field_decl(itervar, create.primitive_type(PrimitiveSort.Integer), constant(0)),
      less(plus(name(tid), mult(name(itervar), opt.tileSize)), copy_rw.rewrite(upperBoundNode)),
      create.assignment(name(itervar), plus(name(itervar), constant(1))),
      rewrite(parBody),
      null,
      cbLoop.getContract(false),
    ))


    val newParTid = List(
      DeclarationStatement(
        parTid.name,
        rewrite(parTid.`type`),
        Some(create.expression(RangeSeq, constant(0), rewrite(opt.tileSize))))
    )
    val newParLabel = parBlock.label

    val newParBlock = create.parallel_block(newParLabel, newParContract, newParTid.toArray, newParBody)
    //TODO OS the region contract has to be rewritten according
    //  to the rules for rewriting the method contract.c
    val newParRegion = m.getBody.asInstanceOf[BlockStatement].getStatement(0) match {
      case pb: ParallelRegion => create.region(rewrite(pb.fuse), rewrite(region.contract), newParBlock)
      case parinv: ParallelInvariant => {

        var newinvs:ASTNode = create.constant(true)

        ASTUtils.conjuncts(parinv.inv, And, Star).forEach {
          case bindexpr: BindingExpression
            if bindexpr.binder == Binder.Star || bindexpr.binder == Binder.Forall =>

            findBounds(bindexpr.select, name(bindexpr.decls.head.name)) match {
              case Some(bounds) => {
                val j = bindexpr.decls.head.name + "_1"
                val i = "i_0"
                val newSelect = and(
                  and(
                    gte(name(j), bounds._1),
                    less(name(j), invoke(null, ceilingFuncName, bounds._2, opt.tileSize))
                  ),
                  less(
                    invoke(null, newIdxFuncName, name(i), name(j), opt.tileSize),
                    bounds._2
                  )
                )
                val newMain = ASTUtils.replace(name(bindexpr.decls.head.name), invoke(null, newIdxFuncName, name(i), name(j), opt.tileSize), bindexpr.main)
                val innerForall = create.binder(bindexpr.binder,
                  copy_rw.rewrite(bindexpr.result_type),
                  Array(create.field_decl(j, copy_rw.rewrite(bindexpr.decls.head.`type`))),
                  Array.empty[Array[ASTNode]],
                  newSelect, // ASTNode selection,
                  newMain // ASTNode main
                )

                val outerLoop = create.binder(bindexpr.binder,
                  copy_rw.rewrite(bindexpr.result_type),
                  Array(create.field_decl(i, copy_rw.rewrite(bindexpr.decls.head.`type`))),
                  Array.empty[Array[ASTNode]],
                  and(
                    lte(constant(0), name(i)),
                    less(name(i), opt.tileSize)
                  ),
                  innerForall
                )
                newinvs = star(newinvs, outerLoop)
              }
              case None => newinvs = star(newinvs, copy_rw.rewrite(bindexpr))
            }
          case pre =>
            newinvs = star(newinvs, copy_rw.rewrite(pre))
        }



        create.invariant_block(parinv.label, newinvs, create.block(create.region(rewrite(region.fuse), rewrite(region.contract), newParBlock)))
      }
      case _ => ???
    }

    val cbMethod = new ContractBuilder()
    cbMethod.appendInvariant(copy_rw.rewrite(m.getContract().invariant))
    cbMethod.appendKernelInvariant(copy_rw.rewrite(m.getContract().kernelInvariant))
    cbMethod.given(copy_rw.rewrite(m.getContract().given):_*)
    cbMethod.yields(copy_rw.rewrite(m.getContract().`yields`):_*)
    ASTUtils.conjuncts(m.getContract().pre_condition, And, Star).forEach {
      case bindexpr: BindingExpression
        if bindexpr.binder == Binder.Star || bindexpr.binder == Binder.Forall =>

        findBounds(bindexpr.select, name(bindexpr.decls.head.name)) match {
          case Some(bounds) => {
            val j = bindexpr.decls.head.name + "_1"
            val i = "i_0"
            val newSelect = and(
              and(
                gte(name(j), bounds._1),
                less(name(j), invoke(null, ceilingFuncName, bounds._2, opt.tileSize))
              ),
              less(
                invoke(null, newIdxFuncName, name(i), name(j), opt.tileSize),
                bounds._2
              )
            )
            val newMain = ASTUtils.replace(name(bindexpr.decls.head.name), invoke(null, newIdxFuncName, name(i), name(j), opt.tileSize), bindexpr.main)
            val innerForall = create.binder(bindexpr.binder,
              copy_rw.rewrite(bindexpr.result_type),
              Array(create.field_decl(j, copy_rw.rewrite(bindexpr.decls.head.`type`))),
              Array.empty[Array[ASTNode]],
              newSelect, // ASTNode selection,
              newMain // ASTNode main
            )

            val outerLoop = create.binder(bindexpr.binder,
              copy_rw.rewrite(bindexpr.result_type),
              Array(create.field_decl(i, copy_rw.rewrite(bindexpr.decls.head.`type`))),
              Array.empty[Array[ASTNode]],
              and(
                lte(constant(0), name(i)),
                less(name(i), opt.tileSize)
              ),
              innerForall
            )
            cbMethod.requires(outerLoop)
          }
          case None => cbMethod.requires(copy_rw.rewrite(bindexpr))
        }
      case pre =>
        cbMethod.requires(copy_rw.rewrite(pre))
    }

    ASTUtils.conjuncts(m.getContract().post_condition, And, Star).forEach {
      case bindexpr: BindingExpression
        if bindexpr.binder == Binder.Star || bindexpr.binder == Binder.Forall =>

        findBounds(bindexpr.select, name(bindexpr.decls.head.name)) match {
          case Some(bounds) => {
            val j = bindexpr.decls.head.name + "_1"
            val i = "i_0"
            val newSelect = and(
              and(
                gte(name(j), bounds._1),
                less(name(j), invoke(null, ceilingFuncName, bounds._2, opt.tileSize))
              ),
              less(
                invoke(null, newIdxFuncName, name(i), name(j), opt.tileSize),
                bounds._2
              )
            )
            val newMain = ASTUtils.replace(name(bindexpr.decls.head.name), invoke(null, newIdxFuncName, name(i), name(j), opt.tileSize), bindexpr.main)
            //TODO OS make a rewrite method for Arrays
            //            val newTriggers = bindexpr match {
            //              case null => null
            //              case _ => bindexpr.triggers.map(n => n.map(n1 => copy_rw.rewrite(n1)).toArray).toArray
            //            }
            val innerForall = create.binder(bindexpr.binder,
              copy_rw.rewrite(bindexpr.result_type),
              Array(create.field_decl(j, copy_rw.rewrite(bindexpr.decls.head.`type`))),
              Array.empty[Array[ASTNode]],
              newSelect, // ASTNode selection,
              newMain // ASTNode main
            )

            val outerLoop = create.binder(bindexpr.binder,
              copy_rw.rewrite(bindexpr.result_type),
              Array(create.field_decl(i, copy_rw.rewrite(bindexpr.decls.head.`type`))),
              Array.empty[Array[ASTNode]],
              and(
                lte(constant(0), name(i)),
                less(name(i), opt.tileSize)
              ),
              innerForall
            )
            cbMethod.ensures(outerLoop)
          }
          case None => cbMethod.ensures(copy_rw.rewrite(bindexpr))
        }
      case pre =>
        cbMethod.ensures(copy_rw.rewrite(pre))
    }

    create.method_decl(
      copy_rw.rewrite(m.getReturnType),
      //TO BE CHANGED
      cbMethod.getContract(),
      m.getName,
      copy_rw.rewrite(m.getArgs),
      create.block(newParRegion)
    )
  }

  private def findUpperbound(m: Method, opt: Tiling, tid: String, upperBoundNode: ASTNode) = {
    upperBoundNode match {
      case c: ConstantExpression =>
        c.value.asInstanceOf[IntegerValue].value
      case _ =>
        val possibleUpperBounds = ASTUtils.conjuncts(create.expression(And, m.getContract.invariant, m.getContract.pre_condition), Star, And).asScala
          .map {
            case o: OperatorExpression =>
              o.operator match {
                case EQ if o.first.`match`(upperBoundNode) && o.second.isInstanceOf[ConstantExpression] =>
                  Some(o.second.asInstanceOf[ConstantExpression].value.asInstanceOf[IntegerValue].value)
                case EQ if o.second.`match`(upperBoundNode) && o.first.isInstanceOf[ConstantExpression] =>
                  Some(o.first.asInstanceOf[ConstantExpression].value.asInstanceOf[IntegerValue].value)
                case GTE if o.first.`match`(upperBoundNode) && o.second.isInstanceOf[ConstantExpression] =>
                  Some(o.second.asInstanceOf[ConstantExpression].value.asInstanceOf[IntegerValue].value)
                case GT if o.first.`match`(upperBoundNode) && o.second.isInstanceOf[ConstantExpression] =>
                  Some(o.second.asInstanceOf[ConstantExpression].value.asInstanceOf[IntegerValue].value + 1)
                case LTE if o.second.`match`(upperBoundNode) && o.first.isInstanceOf[ConstantExpression] =>
                  Some(o.first.asInstanceOf[ConstantExpression].value.asInstanceOf[IntegerValue].value)
                case LT if o.second.`match`(upperBoundNode) && o.first.isInstanceOf[ConstantExpression] =>
                  Some(o.first.asInstanceOf[ConstantExpression].value.asInstanceOf[IntegerValue].value + 1)
                case _ => None
              }
            case _ => None
          }
          .filter(_.isDefined)
        if (possibleUpperBounds.isEmpty) {
          Fail("Could not determine that %s is less than the upperbound for the thread id %s. " +
            "Please specify in the contract of the method that the tile size %s is less than the upperbound for %s",
            opt.tileSize, tid, opt.tileSize, tid)
        }

        possibleUpperBounds.map(_.get).max
    }
  }

  def findBounds(node: ASTNode, itervar: NameExpression): Option[(ASTNode, ASTNode)] = {
    var lowerbounds: Set[ASTNode] = Set.empty
    var upperbounds: Set[ASTNode] = Set.empty

    ASTUtils.conjuncts(node, Star, And).forEach {
      case e: OperatorExpression => e.operator match {
        // Lowerbounds
        case LT if e.second.equals(itervar) =>
          lowerbounds ++= Set(e.first)
        case LTE if e.second.equals(itervar) =>
          lowerbounds ++= Set(e.first)
        case GT if e.first.equals(itervar) =>
          lowerbounds ++= Set(e.second)
        case GTE if e.first.equals(itervar) =>
          lowerbounds ++= Set(e.second)

        // Upperbounds
        case GT if e.second.equals(itervar) =>
          upperbounds ++= Set(e.first)
        case GTE if e.second.equals(itervar) =>
          upperbounds ++= Set(e.first)
        case LT if e.first.equals(itervar) =>
          upperbounds ++= Set(e.second)
        case LTE if e.first.equals(itervar) =>
          upperbounds ++= Set(e.second)
        case _ =>
      }
      case _ =>
    }

    if (lowerbounds.size > 1 || lowerbounds.isEmpty || upperbounds.size > 1 || upperbounds.isEmpty) {
      return None
    }

    // a is the lowerbound, b is the upperbound
    val a = lowerbounds.head
    val b = upperbounds.head

    Some(a, b)
  }


  private def tileOnNodeWithLow(opt: Tiling, tid: String, upperBoundNode: ASTNode, forallVarName: String, node: ASTNode, lowerBound: ASTNode): ASTNode = {
    tileOnNodeWithLowAndUpp(opt, tid, upperBoundNode, forallVarName, node, lowerBound, invoke(null, ceilingFuncName, copy_rw.rewrite(upperBoundNode), opt.tileSize))
  }
  private def tileOnNodeWithUpper(opt: Tiling, tid: String, upperBoundNode: ASTNode, forallVarName: String, node: ASTNode, upperBound: ASTNode): ASTNode = {
    tileOnNodeWithLowAndUpp(opt, tid, upperBoundNode, forallVarName, node, constant(0), upperBound)
  }
  private def tileOnNode(opt: Tiling, tid: String, upperBoundNode: ASTNode, forallVarName: String, node: ASTNode): ASTNode = {
    tileOnNodeWithLowAndUpp(opt, tid, upperBoundNode, forallVarName, node, constant(0), invoke(null, ceilingFuncName, copy_rw.rewrite(upperBoundNode), opt.tileSize))
  }
  private def tileOnNodeWithLowAndUpp(opt: Tiling, tid: String, upperBoundNode: ASTNode, forallVarName: String, node: ASTNode, lowerBound: ASTNode, upperBound: ASTNode): ASTNode = {
    create.starall(
      and(
        gte(name(forallVarName), lowerBound),
        and(
          less(name(forallVarName), upperBound),
          less(
            create.invokation(null, null, newIdxFuncName, create.local_name(tid), create.local_name(forallVarName), opt.tileSize),
            copy_rw.rewrite(upperBoundNode)
          )
        )
      ),
      rewrite(node),
      create.field_decl(forallVarName, create.primitive_type(PrimitiveSort.Integer)),
    )
  }

  private def tileOnNodeInterWithUppWithoutAdd(opt: Tiling, tid: String, upperBoundNode: ASTNode, forallVarName: String, node: ASTNode, upperbound: ASTNode): ASTNode = {
    tileOnNodeInterWithLowAndUpp(opt, tid, upperBoundNode, forallVarName, node,
      create.invokation(null, null, lowFuncName, create.local_name(tid), opt.tileSize),
      upperbound,
      addUpperLimit = false
    )
  }
  private def tileOnNodeInterWithLow(opt: Tiling, tid: String, upperBoundNode: ASTNode, forallVarName: String, node: ASTNode, lowerbound: ASTNode): ASTNode = {
    tileOnNodeInterWithLowAndUpp(opt, tid, upperBoundNode, forallVarName, node,
      lowerbound,
      create.invokation(null, null, uppFuncName, create.local_name(tid), opt.tileSize)
    )
  }
  private def tileOnNodeInter(opt: Tiling, tid: String, upperBoundNode: ASTNode, forallVarName: String, node: ASTNode): ASTNode = {
    tileOnNodeInterWithLowAndUpp(opt, tid, upperBoundNode, forallVarName, node,
      create.invokation(null, null, lowFuncName, create.local_name(tid), opt.tileSize),
      create.invokation(null, null, uppFuncName, create.local_name(tid), opt.tileSize)
    )
  }
  private def tileOnNodeInterWithLowAndUpp(opt: Tiling, tid: String, upperBoundNode: ASTNode, forallVarName: String, node: ASTNode, lowerBound: ASTNode, upperBound: ASTNode, addUpperLimit: Boolean = true): ASTNode = {
    val uppBoundForallVar = if (addUpperLimit)
      and(
        less(name(forallVarName), upperBound),
        less(create.local_name(forallVarName), copy_rw.rewrite(upperBoundNode))
      ) else
      less(name(forallVarName), upperBound)

    create.starall(
      and(
        gte(name(forallVarName), lowerBound),
        and(
          less(name(forallVarName), upperBound),
          uppBoundForallVar
        )
      ),
      rewrite(node),
      create.field_decl(forallVarName, create.primitive_type(PrimitiveSort.Integer)),
    )
  }



  override def visit(e: NameExpression): Unit = {
    if (mapTidto.isEmpty) {
      super.visit(e)
    } else if (e.`match`(mapTidto.get._1)) {
      result = copy_rw.rewrite(mapTidto.get._2)
    } else {
      super.visit(e)
    }
  }
}
