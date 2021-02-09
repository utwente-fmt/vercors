package vct.col.rewrite.gpgpuoptimizations

import vct.col.ast.expr.{OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.LoopStatement
import vct.col.ast.stmt.decl.{GPUOptName, ProgramUnit}
import vct.col.ast.util.{ASTUtils, AbstractRewriter}

case class LoopUnroll(override val source: ProgramUnit) extends AbstractRewriter(source) {
//  override def visit(s: LoopStatement): Unit = {
//    if (s.getUnroll == null || s.getUnroll.name != GPUOptName.LoopUnroll) {
//      super.visit(s)
//      return;
//    }
//
//    // The iteration variable, a NameExpression
//    val itervar = s.getUnroll.args.head
//    // The number to unroll, an integer constant
//    val K = s.getUnroll.args(1)
//    //TODO write the type checking for this
//
//    val cond = s.getEntryGuard
//    val condCheck: Boolean = cond match {
//      case e: OperatorExpression => e.operator match {
//        case StandardOperator.LT => e.first.equals(itervar)
//        case StandardOperator.LTE => e.first.equals(itervar)
//        case StandardOperator.GT => e.second.equals(itervar)
//        case StandardOperator.GTE => e.second.equals(itervar)
//        case _ =>
//          Fail("The condition of this loop does not match the pattern for this optimization", cond.getOrigin)
//          false
//      }
//      case _ =>
//        Fail("The condition of this loop does not match the pattern for this optimization", cond.getOrigin)
//        false // Needed because Fail is not a Nothing.
//    }
//
//    if (!condCheck) {
//      Fail("The condition of this loop does not match the pattern for this optimization", cond.getOrigin)
//      return;
//    }
//
//    val invs = s.getContract.invariant
//    val lowerbound = ASTUtils.conjuncts(invs, StandardOperator.Star, StandardOperator.And).forEach {
//      case e: OperatorExpression => e.operator match {
//        case StandardOperator.LT if e.first.equals(itervar) => e.second // Upperbound
//        case StandardOperator.LT if e.second.equals(itervar) => e.first // Lowerbound
//
//        case StandardOperator.LTE if e.first.equals(itervar)=> e.second // Upperbound
//        case StandardOperator.LTE if e.second.equals(itervar)=> e.first // Lowerbound
//
//        case StandardOperator.GT if e.second.equals(itervar) => e.first // Lowerbound
//        case StandardOperator.GT if e.first.equals(itervar) => e.second // Upperbound
//
//        case StandardOperator.GTE if e.second.equals(itervar) => e.first // Lowerbound
//        case StandardOperator.GTE if e.first.equals(itervar) => e.second // Upperbound
//        case _ => null
//      }
//      case _ => null
//    }
//
//      val a = 1+1
//      //TODO list
//    // 1. implement idea below
//    // 2.
//
//    /////////////////////////
//    // Check if can unroll //
//    /////////////////////////
//    // User has given the name of the iteration variable
//    // We have to find it in contract. Somethign like opt loop_unroll i 6
//
//    // init_block or the exprs before this loop have the initial value of i. Search through parent until it is a method
//
//    //    s.getEntryGuard is the condition of the form i < ???  or i > ??? or all other cases.
//
//    // In the contract we have to find the lowerbound and upperbound for i.
//    // For the lower and upperbound, we split the invariant on SO.Star and SO.Plus and try to match against a<= i and all other cases.
//
//    // body or update_block has the iteration variable update step
//
//    // At this point we can generate the method to see if unrolling is possible.
//
//    //////////////////////
//    // Unroll or return //
//    //////////////////////
//
//  }
}
