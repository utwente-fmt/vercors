package vct.col.rewrite.gpgpuoptimizations

import vct.col.ast.expr.{Binder, BindingExpression, OperatorExpression, StandardOperator}
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{BlockStatement, LoopStatement, ParallelAtomic, ParallelBarrier, ParallelBlock, ParallelInvariant}
import vct.col.ast.stmt.decl.{ASTSpecial, DataLocation, DeclarationStatement, Method, ProgramUnit}
import vct.col.ast.stmt.decl.ASTSpecial.Kind._
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.{ASTUtils, AbstractRewriter, ContractBuilder}

import scala.collection.JavaConverters._
import scala.collection.mutable

class GlobalToRegister(override val source: ProgramUnit) extends AbstractRewriter(source) {

  var inParallelBlock = false
  var inLoop = false
  var inAtomic = false
  var excludeNode = false

  var invs = mutable.Map.empty[ASTNode, ASTNode]


  /**
   * A map from ASTNodes to a triple.
   * The triple consists of the new name by which to replace the (key) ASTNode, an integer to keep track of how many
   * times it has been encountered and a flag to keep track of whether it has been written to
   */
  var nodeToNameAndCount = mutable.Map.empty[ASTNode, (String, Int, Boolean)]
  var templateNodeToNameAndCount = mutable.Map.empty[ASTNode, (String, Int, Boolean)]


  override def visit(inv: ParallelInvariant): Unit = {
    invs += (name(inv.label) -> inv.inv)
    super.visit(inv)
    invs -= name(inv.label)
  }

  override def visit(m: Method): Unit = {
    val opts = m.getGpuOpts.asScala.filter(_.isInstanceOf[DataLocation]).map(_.asInstanceOf[DataLocation]).toList
    if (opts.isEmpty) {
      super.visit(m)
      return
    }

    templateNodeToNameAndCount = mutable.Map.empty[ASTNode, (String, Int, Boolean)]

    var counter = 0
    opts.foreach { opt =>
      val name = opt.arrayName
      opt.locations.foreach { loc =>
        val newName = (name + "_reg_" + counter)
          .replaceAll(" ", "")
          .replaceAll("[^A-Za-z0-9]", "_")
          .replaceAll("__", "_")

        templateNodeToNameAndCount += (get(name, loc) -> (newName, 0, false))
        counter += 1
      }
    }

    nodeToNameAndCount = templateNodeToNameAndCount.clone()

    super.visit(m)

    nodeToNameAndCount.filter(_._2._2 == 0).keySet.foreach {
      Warning("No matches found for %s in the program", _)
    }

    nodeToNameAndCount
      .filter(kv => ((kv._2._2 == 1 || kv._2._2 == 2) && kv._2._3) || (kv._2._2 == 1 && !kv._2._3)).foreach { kv =>
      Warning("Fetching the value of %s into the register might not be efficient, because there is only %s usages in the program.", kv._1, kv._2._2.toString)
    }

    nodeToNameAndCount = mutable.Map.empty[ASTNode, (String, Int, Boolean)]
  }

  def rewriteBody(block: BlockStatement, parent: ASTNode): BlockStatement = {

    val splitByBarrier = block.getStatements.foldLeft(Array(Array.empty[ASTNode])) {
      (prev, next) =>
        if (next.isInstanceOf[ParallelBarrier]) {
          (prev :+ Array(next)) :+ Array.empty[ASTNode]
        } else {
          prev(prev.length - 1) = prev(prev.length - 1) :+ next
          prev
        }
    }
    val newBody = create.block()

    val tmpOrigMap = nodeToNameAndCount.clone()

    splitByBarrier.toList.zipWithIndex.foreach { stmts =>
      nodeToNameAndCount = templateNodeToNameAndCount.clone()
      val tmpBody = create.block()
      stmts._1.foreach { stmt =>
        if (stmt.isInstanceOf[ParallelBarrier]) {
          tmpBody.prepend(copy_rw.rewrite(stmt))
        } else {
          tmpBody.add(rewrite(stmt))
        }
      }
      if (!stmts._1.exists(_.isInstanceOf[ParallelBarrier])) {

        nodeToNameAndCount.filter(_._2._2 != 0).foreach {
          keyval =>
            var annotations: ASTNode = null
            if (stmts._2 == 0) {
              //take from parblock or parallelatomic
              annotations = parent match {
                case ParallelBlock(label, contract, iters, block, deps) => contract.pre_condition
                case pa: ParallelAtomic => pa.synclist.map(invs(_)).filter(_ != null).reduce(and)
                case loop: LoopStatement => loop.getContract.pre_condition
              }
            } else if (splitByBarrier.length > 1) {
              //              take from previous barrier
              annotations = splitByBarrier(stmts._2 - 1).head.asInstanceOf[ParallelBarrier].contract.post_condition
            }
            var cond: ASTNode = create.constant(true)
            ASTUtils.conjuncts(annotations, Star, And).forEach {
              case opexpr: OperatorExpression if opexpr.operator == Implies => {
                opexpr.second match {
                  case expr: OperatorExpression if expr.operator == Perm =>
                    if (keyval._1.equals(expr.first)) {
                      cond = and(cond, opexpr.first)
                    }
                  case _ =>
                }
              }
              case binder: BindingExpression if binder.binder == Binder.Star => {
                binder.main match {
                  case expr: OperatorExpression if expr.operator == Perm =>
                    //  we have of form        input [ (tid * 2 + 2) ]
                    //  expr.first is of form  input[i]
                    val replaces = ASTUtils.replace(name(binder.decls.head.name), keyval._1.asInstanceOf[OperatorExpression].second, expr.first)
                    if (keyval._1.equals(replaces)) {
                      val selector = ASTUtils.replace(
                        name(binder.decls.head.name),
                        keyval._1.asInstanceOf[OperatorExpression].second,
                        binder.select)

                      cond = if (cond.equals(create.constant(true))) selector else and(cond, selector)                    }
                  case _ =>
                }
              }
              case _ =>
            }

            val oldNode = keyval._1
            val newName = keyval._2._1
            if (!cond.equals(create.constant(true))) {
              val decl: ASTNode = create.field_decl(newName, oldNode.getType)
              val assign: ASTNode = create.ifthenelse(rewrite(cond), create.assignment(name(newName), oldNode))
              newBody.append(decl)
              newBody.append(assign)
            } else {
              newBody.append(create.field_decl(newName, oldNode.getType, oldNode))
            }
        }
      }

      //////////////////////////////
      //////////////////////////////
      //////////////////////////////
      //////////////////////////////
      tmpBody.forEachStmt { s => newBody.add(copy_rw.rewrite(s)) }
      //////////////////////////////
      //////////////////////////////
      //////////////////////////////
      //////////////////////////////
      if (!stmts._1.exists(_.isInstanceOf[ParallelBarrier]))
        nodeToNameAndCount.filter(_._2._3).foreach {
          keyval =>

            var annotations: ASTNode = null
            if (stmts._2 == 0) {
              //take from parblock or parallelatomic
              annotations = parent match {
                case ParallelBlock(label, contract, iters, block, deps) => contract.pre_condition
                case pa: ParallelAtomic => pa.synclist.map(invs(_)).filter(_ != null).reduce(and)
                case loop: LoopStatement => loop.getContract.pre_condition
              }
            } else if (splitByBarrier.length > 1) {
              //              take from previous barrier
              annotations = splitByBarrier(stmts._2 - 1).head.asInstanceOf[ParallelBarrier].contract.post_condition
            }
            var cond: ASTNode = create.constant(true)
            ASTUtils.conjuncts(annotations, Star, And).forEach {
              case opexpr: OperatorExpression if opexpr.operator == Implies => {
                opexpr.second match {
                  case expr: OperatorExpression if expr.operator == Perm =>
                    if (keyval._1.equals(expr.first)) {
                      cond = and(cond, opexpr.first)
                    }
                  case _ =>
                }
              }
              case binder: BindingExpression if binder.binder == Binder.Star => {
                binder.main match {
                  case expr: OperatorExpression if expr.operator == Perm =>
                    //                  we have of form          input [ (tid * 2 + 2) ]
                    //                  expr.first is of form    input[i]
                    val replaces = ASTUtils.replace(name(binder.decls.head.name), keyval._1.asInstanceOf[OperatorExpression].second, expr.first)
                    if (keyval._1.equals(replaces)) {
                      val selector = ASTUtils.replace(
                        name(binder.decls.head.name),
                        keyval._1.asInstanceOf[OperatorExpression].second,
                        binder.select)
                      cond = if (cond.equals(create.constant(true))) selector else and(cond, selector)
                    }
                  case _ =>
                }
              }
              case _ =>
            }

            val oldNode = keyval._1
            val newName = keyval._2._1
            if (!cond.equals(create.constant(true))) {
              newBody.append(create.ifthenelse(rewrite(cond), create.assignment(oldNode, name(newName))))
            } else {
              newBody.append(create.assignment(oldNode, name(newName)))
            }
        }
    }
    nodeToNameAndCount = tmpOrigMap
    newBody
  }

  override def visit(pa: ParallelAtomic): Unit = {
    val tmp = inAtomic
    inAtomic = true

    val newBody = pa.block match {
      case b: BlockStatement => rewriteBody(b, pa)
      case notablock => rewrite(notablock)
    }
    val invs = rewrite(pa.synclist.toArray)
    result = create.csl_atomic(newBody, invs: _*)

    inAtomic = tmp
  }

  override def visit(s: LoopStatement): Unit = {
    val tmp = inLoop
    inLoop = true

    val newBody = s.getBody match {
      case b: BlockStatement => rewriteBody(b, s)
      case notablock => rewrite(notablock)
    }

    result = create.loop(
      rewrite(s.getInitBlock),
      rewrite(s.getEntryGuard),
      rewrite(s.getExitGuard),
      rewrite(s.getUpdateBlock),
      newBody,
      rewrite(s.getContract)
    )
    inLoop = tmp
  }

  override def visit(pb: ParallelBlock): Unit = {
    val tmp = inParallelBlock
    inParallelBlock = true

    //    val splitByBarrier = pb.block.getStatements.foldLeft(Array(Array.empty[ASTNode])) {
    //      (prev, next) =>
    //        if (next.isInstanceOf[ParallelBarrier]) {
    //          (prev :+ Array(next)) :+ Array.empty[ASTNode]
    //        } else {
    //          prev(prev.length - 1) = prev(prev.length - 1) :+ next
    //          prev
    //        }
    //    }
    //    val newBody = create.block()
    //
    //    val tmpOrigMap = nodeToNameAndCount.clone()
    //
    //    splitByBarrier.foreach { stmts =>
    //      nodeToNameAndCount = tmpOrigMap.clone()
    //      val tmpBody = create.block()
    //      stmts.foreach { stmt =>
    //        if (stmt.isInstanceOf[ParallelBarrier]) {
    //          tmpBody.prepend(copy_rw.rewrite(stmt))
    //        } else {
    //          tmpBody.add(rewrite(stmt))
    //        }
    //      }
    //      if (!stmts.exists(_.isInstanceOf[ParallelBarrier]))
    //        nodeToNameAndCount.filter(_._2._2 != 0).foreach {
    //          keyval =>
    //            val oldNode = keyval._1
    //            val newName = keyval._2._1
    //            newBody.append(new DeclarationStatement(newName, oldNode.getType, oldNode))
    //        }
    //      tmpBody.forEachStmt { s => newBody.add(copy_rw.rewrite(s)) }
    //      // _._2.3 refers to the flag to see whether the ASTNode has been written to.
    //      if (!stmts.exists(_.isInstanceOf[ParallelBarrier]))
    //        nodeToNameAndCount.filter(_._2._3).foreach {
    //          keyval =>
    //            val oldNode = keyval._1
    //            val newName = keyval._2._1
    //            newBody.append(create.assignment(oldNode, name(newName)))
    //        }
    //    }


    val newBody = pb.block match {
      case b: BlockStatement => rewriteBody(pb.block, pb)
      case notablock => rewrite(notablock)
    }

    result = create.parallel_block(
      pb.label,
      copy_rw.rewrite(pb.contract),
      copy_rw.rewrite(pb.itersJava),
      newBody,
      copy_rw.rewrite(pb.deps)
    )

    inParallelBlock = tmp
  }

  override def visit(s: AssignmentStatement): Unit = {
    if (nodeToNameAndCount.contains(s.location)) {
      val (newName, count, writtenTo) = nodeToNameAndCount(s.location)
      nodeToNameAndCount(s.location) = (newName, count, true)
    }
    super.visit(s)
  }

  override def visit(e: OperatorExpression): Unit = {
    if (!inParallelBlock || excludeNode) {
      super.visit(e)
      return
    }

    e.operator match {
      case Subscript => {
        if (nodeToNameAndCount.contains(e)) {
          val (newName, count, writtenTo) = nodeToNameAndCount(e)
          nodeToNameAndCount.remove(e)
          nodeToNameAndCount(e) = (newName, count + 1, writtenTo)
          result = create.local_name(newName)
        } else {
          super.visit(e)
        }
      }
      case Old =>
        exclude(e)
      case Perm =>
        exclude(e)
      case _ => super.visit(e)
    }
  }


  //  override def visit(special: ASTSpecial): Unit = {
  //    special.kind match {
  //      case Assert =>
  //        exclude(special)
  //      case Assume =>
  //        exclude(special)
  //      case Inhale =>
  //        exclude(special)
  //      case Exhale =>
  //        exclude(special)
  //      case _ => super.visit(special)
  //    }
  //
  //  }

  def exclude(node: OperatorExpression): Unit = {
    val tmp = excludeNode
    excludeNode = true
    super.visit(node)
    excludeNode = tmp
  }

  def exclude(node: ASTSpecial): Unit = {
    val tmp = excludeNode
    excludeNode = true
    super.visit(node)
    excludeNode = tmp
  }
}
