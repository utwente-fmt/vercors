package vct.col.util

import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.{ParallelBlock, ParallelInvariant, ParallelRegion}
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.util.{NameScanner, RecursiveVisitor}

import scala.collection.mutable

object LocalVariableChecker {
  def check(pu: ProgramUnit): Unit = {
    pu.accept(new LocalVariableChecker(pu))
  }
}

/**
  * Checks if local variables are not misused. This means:
  * - Only one parallel block can write to a var, or all can read
  * - Vars used in invariants must be effectively final
  */
class LocalVariableChecker(arg: ProgramUnit) extends RecursiveVisitor[Unit](arg) {
  val accessSets: mutable.Map[ASTNode, Set[String]] = mutable.Map[ASTNode, Set[String]]()
  val writeSets: mutable.Map[ASTNode, Set[String]] = mutable.Map[ASTNode, Set[String]]()

  override def visit(region: ParallelRegion): Unit = {
    super.visit(region)

    // Pre-calculate access and write sets
    for (block <- region.blocks) {
      val ns = new NameScanner
      block.accept(ns)
      accessSets.put(block, ns.accesses)
      writeSets.put(block, ns.writes)
    }

    region.blocks.foreach(checkParallelBlock(_, region.blocks))
  }

  def checkParallelBlock(parallelBlock: ParallelBlock, neighbours: List[ParallelBlock]): Unit = {
    if (parallelBlock.iters.isEmpty) {
      // If a parallel block models 1 thread, it may write to stack variables, provided other threads
      // do not access (read/write) it. Implicitly, only one par block can have write permission for a stack var.
      // Conversely, every par block can get a read permission for any var, if none of the par blocks write.
      for (parallelBlockB <- neighbours.filter(_ ne parallelBlock)) {
        val intersection = writeSets(parallelBlock).intersect(accessSets(parallelBlockB))
        if (intersection.nonEmpty) {
          Abort("Parallelblock writes to the following shared variables: %s", intersection.mkString(", "))
        }
      }
    } else {
      // If a parallel block models 1 or more threads, it may not write to stack variables, as this
      // could potentially cause data races between threads.
      if (writeSets(parallelBlock).nonEmpty) {
        val vars = writeSets(parallelBlock).mkString(", ")
        val msg = s"Parallel block modelling multiple threads cannot write to shared stack variables: $vars"
        parallelBlock.getOrigin.report("error", msg)
        Abort("")
      }
    }
  }

  override def visit(invariant: ParallelInvariant): Unit = {
    super.visit(invariant)

    val readOnlyNames = NameScanner.accesses(invariant.inv)
    val writeSet = NameScanner.writes(invariant.block)

    val badWrites = readOnlyNames.intersect(writeSet)

    if (badWrites.nonEmpty) {
      invariant.getOrigin.report("error", "The following vars appear in the invariant but are also written to in the invariant body: %s", badWrites.mkString(", "))
      Abort("Invariant writes to local variables that appear in invariant")
    }
  }
}
