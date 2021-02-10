package vct.col.rewrite.gpgpuoptimizations

import hre.lang.System.Progress
import vct.col.ast.expr.constant.ConstantExpression
import vct.col.ast.expr.{OperatorExpression, StandardOperator}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.LoopStatement
import vct.col.ast.stmt.decl.{Contract, GPUOptName, ProgramUnit}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.{ASTUtils, AbstractRewriter}

import scala.language.postfixOps

case class LoopUnroll(override val source: ProgramUnit) extends AbstractRewriter(source) {

  private var inLoop: Boolean = false
  private var itervar: ASTNode = null


  override def visit(s: LoopStatement): Unit = {
//    val body = super.rewrite(s.getBody)
    if (s.getUnroll == null || s.getUnroll.name != GPUOptName.LoopUnroll) {
      super.visit(s)
      return;
    } else if (inLoop) {
      Warning("Only one loop can be optimized at a time. By default, the outer loop is optimized first. Please run VerCors again with the output to optimize the inner loops %s", s.getOrigin)
      super.visit(s)
      return;
    }
    inLoop = true

    // The iteration variable, a NameExpression
    itervar = s.getUnroll.args.head
    // The number to unroll, an integer constant
    val K = s.getUnroll.args(1)
    //TODO write the type checking for this


    //////////////////////////////////////////////
    /// check if condition matches our pattern ///
    //////////////////////////////////////////////
    val cond = s.getEntryGuard
    val condCheck: Boolean = cond match {
      case e: OperatorExpression => e.operator match {
        case StandardOperator.LT => e.first.equals(itervar)
        case StandardOperator.LTE => e.first.equals(itervar)
        case StandardOperator.GT => e.second.equals(itervar)
        case StandardOperator.GTE => e.second.equals(itervar)
        case StandardOperator.EQ => e.first.equals(itervar) ^ e.second.equals(itervar)
        case StandardOperator.NEQ => e.first.equals(itervar) ^ e.second.equals(itervar)

          //TODO which cases do we need
        case _ =>
          Fail("The condition of this loop does not match the pattern for this optimization", cond.getOrigin)
          false
      }
      case _ =>
        Fail("The condition of this loop does not match the pattern for this optimization", cond.getOrigin)
        false // Needed because Fail is not a Nothing.
    }

    if (!condCheck) {
      Fail("The condition of this loop does not match the pattern for this optimization", cond.getOrigin)
      return;
    }

    /////////////////////////////////
    /// find the update statement ///
    /////////////////////////////////
    s.getBody().apply(this)
    if (s.getExitGuard != null) s.getExitGuard.apply(this)

    if (updateStmnt == null) {
      Fail("The iteration variable %s is never updated.", itervar)
    }
//    Progress("Update Statement: %s in", updateStmnt)


    //////////////////////////////////////
    /// find the lower and upperbounds ///
    //////////////////////////////////////

    val invs = s.getContract.invariant
    var lowerbounds:Set[ASTNode] = Set.empty
    var upperbounds:Set[ASTNode] = Set.empty

    ASTUtils.conjuncts(invs, StandardOperator.Star, StandardOperator.And).forEach {
      case e: OperatorExpression => e.operator match {
        case StandardOperator.LT if e.second.equals(itervar) => lowerbounds ++= Set(create.expression(StandardOperator.Plus, e.first, create.constant(1))) // Lowerbound
        case StandardOperator.LTE if e.second.equals(itervar)=> lowerbounds ++= Set(e.first) // Lowerbound
        case StandardOperator.GT if e.second.equals(itervar) => lowerbounds ++= Set(create.expression(StandardOperator.Plus, e.first, create.constant(1))) // Lowerbound
        case StandardOperator.GTE if e.second.equals(itervar) => lowerbounds ++= Set(e.first) // Lowerbound
          
        case StandardOperator.LT if e.first.equals(itervar) => upperbounds ++= Set(create.expression(StandardOperator.Minus, e.second, create.constant(1))) // Upperbound
        case StandardOperator.LTE if e.first.equals(itervar)=> upperbounds ++= Set(e.second) // Upperbound
        case StandardOperator.GT if e.first.equals(itervar) => upperbounds ++= Set(create.expression(StandardOperator.Minus, e.second, create.constant(1))) // Upperbound
        case StandardOperator.GTE if e.first.equals(itervar) => upperbounds ++= Set(e.second) // Upperbound
        case _ => null
      }
      case _ => null
    }

    if (lowerbounds.size > 1) {
      Fail("Multiple matches for the specification of the lowerbound for the loop variable %s %s.", itervar, lowerbounds)
    } else if (lowerbounds.isEmpty) {
      Fail("No matches for the specification of the lowerbound for the loop variable %s.", itervar)
    } else if (upperbounds.size > 1) {
      Fail("Multiple matches for the specification of the upperbound for the loop variable %s %s.", itervar, upperbounds)
    } else if (upperbounds.isEmpty) {
      Fail("No matches for the specification of the upperbound for the loop variable %s.", itervar)
    }

    val a = lowerbounds.head
    val b = upperbounds.head

    Progress("Lowerbound = %s", a)
    Progress("Upperbound = %s", b)

    /**TODO Questions to ask
     * Ask Pieter how to best match against a part of the AST.
     * Ask for the pass that extract variable declarations from the program.
     */


    /**TODO check if you can actually unroll
     * 1. generate method for checking unroll
     * 2. Send to Viper
     * 3. Process output
     */

//    result is K bodies + loop+updated invariants
    result = create.loop(
      rewrite(s.getInitBlock),
      rewrite(s.getEntryGuard),
      rewrite(s.getExitGuard),
      rewrite(s.getUpdateBlock),
      //TODO the declarations in the body must be extracted
      rewrite(s.getBody),
      null
    )


    /////////////////////////
    // Check if can unroll //
    /////////////////////////
    // User has given the name of the iteration variable
    // [DONE]We have to find it in contract. Somethign like gpuopt loop_unroll i 6

    // [SKIPPED]: init_block or the exprs before this loop have the initial value of i. Search through parent until it is a method

    // [DONE]   s.getEntryGuard is the condition of the form i < ???  or i > ??? or all other cases.

    // [DONE] In the contract we have to find the lowerbound and upperbound for i.
    // For the lower and upperbound, we split the invariant on SO.Star and SO.Plus and try to match against a<= i and all other cases.

    // [DONE] body or update_block has the iteration variable update step


    // At this point we can generate the method to see if unrolling is possible.

    //////////////////////
    // Unroll or return //
    //////////////////////
    inLoop = false

  }

  private var updateStmnt: (StandardOperator, ASTNode) = null
  

  override def visit(o: OperatorExpression): Unit = {
    if (!inLoop || !o.first.equals(itervar)) {
      super.visit(o);
      return
    } else if (updateStmnt != null) {
      Fail("Multiple update statements for iteration variable %s. Only one update statement is allowed.", itervar)
    }

    o.operator match {
      //TODO add cases for i += -C (and for -=, *=, /=)
      case StandardOperator.AddAssign =>
        updateStmnt = (StandardOperator.Plus, o.second)
      case StandardOperator.PostIncr =>
        updateStmnt = (StandardOperator.Plus, create constant(1))
      case StandardOperator.PostDecr =>
        updateStmnt = (StandardOperator.Minus, create constant(1))
      case StandardOperator.SubAssign =>
        updateStmnt = (StandardOperator.Minus, o.second)
      case StandardOperator.MulAssign =>
        updateStmnt = (StandardOperator.Mult, o.second)
      case StandardOperator.DivAssign =>
        updateStmnt = (StandardOperator.Div, o.second)
      case _ =>
    }
  }

  override def visit(s: AssignmentStatement): Unit = {
    if (!inLoop || !s.location.equals(itervar)) {
      super.visit(s);
      return;
    } else if (updateStmnt != null) {
      Fail("Multiple update statements for iteration variable %s. Only one update statement is allowed.", itervar)
    }

    s.expression match {
      case e: OperatorExpression => e.operator match {
        case StandardOperator.Plus if e.first.equals(itervar) =>
           e.second match {
            case c:ConstantExpression =>
              updateStmnt = (StandardOperator.Plus, e.second)
            case o: OperatorExpression if o.operator == StandardOperator.UMinus && o.first.isInstanceOf[ConstantExpression] =>
              updateStmnt = (StandardOperator.Minus, o.first)
          }
        case StandardOperator.Plus if e.second.equals(itervar) =>
          e.first match {
            case c:ConstantExpression =>
              updateStmnt = (StandardOperator.Plus, e.first)
            case o: OperatorExpression if o.operator == StandardOperator.UMinus && o.first.isInstanceOf[ConstantExpression] =>
              updateStmnt = (StandardOperator.Minus, o.first)
          }
        case (StandardOperator.Minus) if e.first.equals(itervar) =>
          updateStmnt = (e.operator, e.second)
        case (StandardOperator.Minus) if e.second.equals(itervar) =>
          updateStmnt = (e.operator, e.first)
        case (StandardOperator.FloorDiv) if e.first.equals(itervar) =>
          updateStmnt = (e.operator, e.second)
//        case (StandardOperator.FloorDiv) if e.second.equals(itervar) =>
          //TODO should there be a warning here
        //          updateOp = e.operator
//          updateConstant = e.first
        case (StandardOperator.Mult) if e.first.equals(itervar) =>
          updateStmnt = (e.operator, e.second)
        case (StandardOperator.Mult) if e.second.equals(itervar) =>
          updateStmnt = (e.operator, e.first)
        case _ => Warning("%s is not supported in the update statement at %s.", e.operator, e.getOrigin)
      }
      case _ =>
    }

    // C is a ConstantExpression
    // -C is a OperatorExpression UMinus C
    //TODO treat -C + i as i - C

  }
}
