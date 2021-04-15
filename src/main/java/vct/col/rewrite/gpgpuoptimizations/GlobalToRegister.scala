package vct.col.rewrite.gpgpuoptimizations

import vct.col.ast.expr.OperatorExpression
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{LoopStatement, ParallelBlock}
import vct.col.ast.stmt.decl.{ASTSpecial, DataLocation, DeclarationStatement, Method, ProgramUnit}
import vct.col.ast.stmt.decl.ASTSpecial.Kind._
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.{ASTUtils, AbstractRewriter, ContractBuilder}

import scala.collection.JavaConverters._
import scala.collection.mutable

class GlobalToRegister(override val source: ProgramUnit) extends AbstractRewriter(source) {

  var inParallelBlock = false
  var excludeNode = false

  /**
   * A map from ASTNodes to a triple.
   * The triple consists of the new name by which to replace the (key) ASTNode, an integer to keep track of how many
   *  times it has been encountered and a flag to keep track of whether it has been written to
   */
  var nodeToNameAndCount = mutable.Map.empty[ASTNode, (String, Int, Boolean)]

  override def visit(m: Method): Unit = {
    val opts = m.getGpuOpts.asScala.filter(_.isInstanceOf[DataLocation]).map(_.asInstanceOf[DataLocation]).toList
    if (opts.isEmpty) {
      super.visit(m)
      return
    }

    nodeToNameAndCount = mutable.Map.empty[ASTNode, (String,Int, Boolean) ]

    var counter = 0
    opts.foreach { opt =>
      val name = opt.arrayName
      opt.locations.foreach { loc =>
        val newName = (name + "_reg_" + counter)
          .replaceAll(" ", "")
          .replaceAll("[^A-Za-z0-9]", "_")
          .replaceAll("__", "_")

        nodeToNameAndCount += (get(name, loc) -> (newName, 0, false))
        counter += 1
      }
    }

    super.visit(m)

    nodeToNameAndCount.filter(_._2._2 == 0).keySet.foreach {
      Warning("No matches found for %s in the program", _)
    }

    nodeToNameAndCount
      .filter(kv => ((kv._2._2 == 1 || kv._2._2 == 2) && kv._2._3) || (kv._2._2 == 1 && !kv._2._3)).foreach { kv =>
      Warning("Fetching the value of %s into the register might not be efficient, because there is only %s usages in the program.", kv._1, kv._2._2.toString)
    }

    nodeToNameAndCount = mutable.Map.empty[ASTNode, (String,Int, Boolean) ]
  }

  override def visit(s: LoopStatement): Unit = {
    if (!inParallelBlock) {
      super.visit(s)
      return
    }

    val cb = new ContractBuilder
    ASTUtils.conjuncts(s.getContract.invariant, Star, And).forEach {
      case o: OperatorExpression if o.operator == Perm && nodeToNameAndCount.contains(o.first) =>
      case inv => cb.appendInvariant(rewrite(inv))
    }

    cb.appendKernelInvariant(rewrite(s.getContract.kernelInvariant))
    cb.requires(rewrite(s.getContract.pre_condition))
    cb.ensures(rewrite(s.getContract.post_condition))

    result = create.loop(
      rewrite(s.getInitBlock),
      rewrite(s.getEntryGuard),
      rewrite(s.getExitGuard),
      rewrite(s.getUpdateBlock),
      rewrite(s.getBody),
      cb.getContract()
    )

  }

  override def visit(pb: ParallelBlock): Unit = {
    val tmp = inParallelBlock
    inParallelBlock = true

    val newBody = rewrite(pb.block)
    nodeToNameAndCount.filter(_._2._2 != 0).foreach {
      keyval =>
        val oldNode = keyval._1
        val newName = keyval._2._1
        newBody.prepend(new DeclarationStatement(newName, oldNode.getType, oldNode))
    }

    // _._2.3 refers to the flag to see whether the ASTNode has been written to.
    nodeToNameAndCount.filter(_._2._3).foreach {
      keyval =>
      val oldNode = keyval._1
      val newName = keyval._2._1
      newBody.append(create.assignment(oldNode, name(newName)))
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


  override def visit(special: ASTSpecial): Unit = {
    special.kind match {
      case Assert =>
        exclude(special)
      case Assume =>
        exclude(special)
      case Inhale =>
        exclude(special)
      case Exhale =>
        exclude(special)
      case _ => super.visit(special)
    }

  }

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
