package vct.col.util

import vct.col.ast.stmt.composite.{ParallelBlock, ParallelInvariant, ParallelRegion}
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.util.RecursiveVisitor

object LocalVariableChecker {
  def check(pu: ProgramUnit): Unit = {
    pu.accept(new LocalVariableChecker(pu));
  }
}

/**
  * Checks if local variables are not misused. This means:
  * - Only one parallel block can write to a var, or all can read
  * - Vars used in invariants must be effectively final
  */
class LocalVariableChecker(arg: ProgramUnit) extends RecursiveVisitor[Unit](arg) {
  private def getAccessSet(arg: Any): Set[String] = ???
  private def getWriteSet(arg: Any): Set[String] = ???
  private def getFreeNames(arg: Any): Set[String] = ???

  override def visit(region: ParallelRegion): Unit = {
    // Check that: all may read, or one may write

    val accessSets = region.blocks.map(parallelBlock => (parallelBlock, getAccessSet(parallelBlock))).toMap
    val writeSets = region.blocks.map(parallelBlock => (parallelBlock, getWriteSet(parallelBlock))).toMap

    for (parallelBlockA <- region.blocks) {
      for (parallelBlockB <- region.blocks) {
        if (parallelBlockA ne parallelBlockB) {
          val intersection = writeSets(parallelBlockA).intersect(accessSets(parallelBlockB))
          if (intersection.nonEmpty) {
            Abort("Parallelblock on line %s writes to the following variables that are accessed by parallel block on line %s: %s",
              parallelBlockA.getOrigin,
              parallelBlockB.getOrigin,
              intersection
            )
          }
        }
      }
    }
  }

  override def visit(invariant: ParallelInvariant): Unit = {
    val readOnlyNames = getFreeNames(invariant.inv)
    val writeSet = getWriteSet(invariant.block)

    val badWrites = readOnlyNames.intersect(writeSet)

    if (badWrites.nonEmpty) {
      invariant.getOrigin.report("error", "The following vars appear in the invariant but are also writting to in the invariant body: %s", badWrites);
      Abort("Invariant writes to local variables that appear in invariant")
    }
  }
}
