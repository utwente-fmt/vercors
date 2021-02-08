package vct.col.rewrite.gpgpuoptimizations

import vct.col.ast.stmt.composite.LoopStatement
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.util.AbstractRewriter

case class LoopUnroll(override val source: ProgramUnit) extends AbstractRewriter(source) {
  override def visit(s: LoopStatement): Unit = {
//    private ContractBuilder cb=new ContractBuilder();
//    private Contract contract;
//    private ASTNode body;
//    private ASTNode entry_guard;
//    private ASTNode exit_guard;
//    private ASTNode init_block;
//    private ASTNode update_block;

    //TODO list
    // 1. implement idea below
    // 2. Do we need a new ASTNode for this?
    // 3.

    /////////////////////////
    // Check if can unroll //
    /////////////////////////
    // User has given the name of the iteration variable
    // We have to find it in contract. Somethign like opt loop_unroll i 6

    // init_block or the exprs before this loop have the initial value of i. Search through parent until it is a method

    //    s.getEntryGuard is the condition of the form i < ???  or i > ??? or all other cases.

    // In the contract we have to find the lowerbound and upperbound for i.
    // For the lower and upperbound, we split the invariant on SO.Star and SO.Plus and try to match against a<= i and all other cases.

    // body or update_block has the iteration variable update step

    // At this point we can generate the method to see if unrolling is possible.

    //////////////////////
    // Unroll or return //
    //////////////////////

  }
}
