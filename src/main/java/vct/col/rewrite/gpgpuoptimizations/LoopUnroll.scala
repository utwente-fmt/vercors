package vct.col.rewrite.gpgpuoptimizations

import hre.lang.System.Progress
import vct.col.ast.expr.constant.{ConstantExpression, IntegerValue}
import vct.col.ast.expr.{OperatorExpression, StandardOperator}
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.LoopStatement
import vct.col.ast.stmt.decl.{Contract, GPUOptName, ProgramUnit}
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.{ASTUtils, AbstractRewriter, ContractBuilder}

import scala.language.postfixOps
case class LoopUnroll(override val source: ProgramUnit) extends AbstractRewriter(source) {

  private var inLoop: Boolean = false
  private var itervar: ASTNode = null


  override def visit(s: LoopStatement): Unit = {
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
    val K:ConstantExpression = s.getUnroll.args(1).asInstanceOf[ConstantExpression]
    //TODO write the type checking for this


    //////////////////////////////////////////////
    /// check if condition matches our pattern ///
    //////////////////////////////////////////////
    val cond = s.getEntryGuard
    val condCheck: Boolean = cond match {
      case e: OperatorExpression => e.operator match {
        case LT => e.first.equals(itervar)
        case LTE => e.first.equals(itervar)
        case GT => e.second.equals(itervar)
        case GTE => e.second.equals(itervar)
        case EQ => e.first.equals(itervar) ^ e.second.equals(itervar)
        case NEQ => e.first.equals(itervar) ^ e.second.equals(itervar)

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
    val body = rewrite(s.getBody())
    if (s.getExitGuard != null) s.getExitGuard.apply(this)

    if (updateStmnt == null) {
      Fail("The iteration variable %s is never updated.", itervar)
    }
//    Progress("Update Statement: %s in", updateStmnt)


    //////////////////////////////////////
    /// find the lower and upperbounds ///
    //////////////////////////////////////

    val cb = new ContractBuilder
    val invs = s.getContract.invariant
    var lowerbounds:Set[ASTNode] = Set.empty
    var upperbounds:Set[ASTNode] = Set.empty

    ASTUtils.conjuncts(invs, Star, And).forEach {
      case e: OperatorExpression => e.operator match {
        case LT if e.second.equals(itervar) =>
          cb.appendInvariant(rewrite(e))
          lowerbounds ++= Set(create.expression(Plus, e.first, create.constant(1))) // Lowerbound
        case LTE if e.second.equals(itervar)=>
          if (updateStmnt._1 == Plus) {
            val updatedLowerbound = create.expression(Plus, e.first, create.expression(Mult, rewrite(updateStmnt._2), rewrite(K)))
            val updatedInvariant = create.expression(LTE, updatedLowerbound, rewrite(e.second))
            cb.appendInvariant(updatedInvariant)
          } else {
            //TODO this case is for debug purposes
            cb.appendInvariant(rewrite(e))
          }

          lowerbounds ++= Set(e.first) // Lowerbound
        case GT if e.second.equals(itervar) =>
          cb.appendInvariant(rewrite(e))
          lowerbounds ++= Set(create.expression(Plus, e.first, create.constant(1))) // Lowerbound
        case GTE if e.second.equals(itervar) =>
          cb.appendInvariant(rewrite(e))
          lowerbounds ++= Set(e.first) // Lowerbound
        case LT if e.first.equals(itervar) =>
          cb.appendInvariant(rewrite(e))
          upperbounds ++= Set(create.expression(Minus, e.second, create.constant(1))) // Upperbound
        case LTE if e.first.equals(itervar)=>
          cb.appendInvariant(rewrite(e))
          upperbounds ++= Set(e.second) // Upperbound
        case GT if e.first.equals(itervar) =>
          cb.appendInvariant(rewrite(e))
          upperbounds ++= Set(create.expression(Minus, e.second, create.constant(1))) // Upperbound
        case GTE if e.first.equals(itervar) =>
          cb.appendInvariant(rewrite(e))
          upperbounds ++= Set(e.second) // Upperbound
        case _ => cb.appendInvariant(rewrite(e))
      }
      case default => cb.appendInvariant(rewrite(default))
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
     * [For the precprocessing phase] Ask for the pass that extract variable declarations from the program.
     * Do we have a composite statement list?
     */


    /**TODO check if you can actually unroll
     * 1. generate method for checking unroll
     * 2. Send to Viper
     * 3. Process output
     */

//    result is K bodies + loop+updated invariants
    val bodiesPlusLoop = Seq.fill(K.value.asInstanceOf[IntegerValue].value)(new AbstractRewriter(this.source).rewrite(s.getBody)) ++
      Seq(create.loop(
          rewrite(s.getInitBlock),
          rewrite(s.getEntryGuard),
          rewrite(s.getExitGuard),
          rewrite(s.getUpdateBlock),
          //TODO the declarations in the body must be extracted
          body,
          cb.getContract(false)
        ))
    result = create.block(bodiesPlusLoop: _*)


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
    if (!inLoop || !o.first.equals(itervar) || !Set(AddAssign, PostIncr, PostDecr, SubAssign, MulAssign, DivAssign).contains(o.operator)) {
      super.visit(o)
      return
    } else if (updateStmnt != null) {
      Fail("Multiple update statements for iteration variable %s. Only one update statement is allowed.", itervar)
    }

    o.operator match {
      //TODO add cases for i += -C (and for -=, *=, /=)
      case AddAssign =>
        updateStmnt = (Plus, o.second)
      case PostIncr =>
        updateStmnt = (Plus, create constant(1))
      case PostDecr =>
        updateStmnt = (Minus, create constant(1))
      case SubAssign =>
        updateStmnt = (Minus, o.second)
      case MulAssign =>
        updateStmnt = (Mult, o.second)
      case DivAssign =>
        updateStmnt = (Div, o.second)
      case _ =>
    }

    super.visit(o)
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
        case Plus if e.first.equals(itervar) =>
           e.second match {
            case c:ConstantExpression =>
              updateStmnt = (Plus, e.second)
            case o: OperatorExpression if o.operator == UMinus && o.first.isInstanceOf[ConstantExpression] =>
              updateStmnt = (Minus, o.first)
          }
        case Plus if e.second.equals(itervar) =>
          e.first match {
            case c:ConstantExpression =>
              updateStmnt = (Plus, e.first)
            case o: OperatorExpression if o.operator == UMinus && o.first.isInstanceOf[ConstantExpression] =>
              updateStmnt = (Minus, o.first)
          }
        case (Minus) if e.first.equals(itervar) =>
          updateStmnt = (e.operator, e.second)
        case (Minus) if e.second.equals(itervar) =>
          updateStmnt = (e.operator, e.first)
        case (FloorDiv) if e.first.equals(itervar) =>
          updateStmnt = (e.operator, e.second)
//        case (FloorDiv) if e.second.equals(itervar) =>
          //TODO should there be a warning here
        //          updateOp = e.operator
//          updateConstant = e.first
        case (Mult) if e.first.equals(itervar) =>
          updateStmnt = (e.operator, e.second)
        case (Mult) if e.second.equals(itervar) =>
          updateStmnt = (e.operator, e.first)
        case _ => Warning("%s is not supported in the update statement at %s.", e.operator, e.getOrigin)
      }
      case _ =>
    }

    super.visit(s)
    // C is a ConstantExpression
    // -C is a OperatorExpression UMinus C
    //TODO treat -C + i as i - C

  }
}
