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
  override def visit(region: ParallelRegion): Unit = {
    // Check that: all may read, or one may write

    val accessSets = mutable.Map[ASTNode, Set[String]]()
    val writeSets = mutable.Map[ASTNode, Set[String]]()

    for (block <- region.blocks) {
      val ns = new NameScanner
      block.accept(ns)
      accessSets.put(block, ns.accesses)
      writeSets.put(block, ns.writes)
    }

    for (parallelBlockA <- region.blocks) {
      if (parallelBlockA.iters.isEmpty) {
        // If a parallel block models 1 thread, it may write to stack variables, provided other threads
        // do not access (read/write) it
        for (parallelBlockB <- region.blocks.filter(_ ne parallelBlockA)) {
          val intersection = writeSets(parallelBlockA).intersect(accessSets(parallelBlockB))
          if (intersection.nonEmpty) {
            Abort("Parallelblock writes to the following shared variables: %s", intersection.mkString(", "))
          }
        }
      } else {
        // If a parallel block models 1 or more threads, it may not write to stack variables, as this
        // could potentially cause data races between threads
        if (writeSets(parallelBlockA).nonEmpty) {
          val msg = "Parallel block modelling multiple threads cannot write to shared stack variables"
          parallelBlockA.getOrigin.report("error", msg)
          Abort("")
        }
      }
    }
  }

  override def visit(invariant: ParallelInvariant): Unit = {
    val readOnlyNames = NameScanner.accesses(invariant.inv)
    val writeSet = NameScanner.writes(invariant.block)

    val badWrites = readOnlyNames.intersect(writeSet)

    if (badWrites.nonEmpty) {
      invariant.getOrigin.report("error", "The following vars appear in the invariant but are also writting to in the invariant body: %s", badWrites.mkString(", "))
      Abort("Invariant writes to local variables that appear in invariant")
    }
  }
}
