package vct.col.util

import vct.col.ast.stmt.composite.{ParallelInvariant, ParallelRegion}
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.util.RecursiveVisitor

class InvariantLocalsChecker(arg: ProgramUnit) extends RecursiveVisitor[Unit](arg) {
  override def visit(region: ParallelRegion): Unit = {
    // End verdict: All may read, or one may write

    // Intersection of all write sets of blocks must be empty

    // Read/write sets consist of only free variables.

    // Intersection of all write sets must be empty
    // assert intersect(foreach(NameScanner.scan(region[i]).writes)).size() == 0;

    // Writes of regions cannot be reads in other regions:
    // For each region i:
      // For each other region j:
        // intersect(i.writes, j.reads).size == 0;

    // Free variables in region that are written to
    val freeWriteVars: Set[String] = ???;
    // Free variables in region that are read from
    val freeReadVars: Set[String] = ???;
  }
}
