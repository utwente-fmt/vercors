package vct.col.rewrite.gpgpuoptimizations

import vct.col.ast.expr.{OperatorExpression, StandardOperator}
import vct.col.ast.expr.StandardOperator._
import vct.col.ast.expr.constant.ConstantExpression
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.ast.stmt.terminal.AssignmentStatement
import vct.col.ast.util.AbstractRewriter

case class FindUpdateStatement(override val source: ProgramUnit, itervar: ASTNode) extends AbstractRewriter(source) {

  var updateStmnt: Option[(StandardOperator, ASTNode)] = None

  override def visit(o: OperatorExpression): Unit = {
    if (!o.first.equals(itervar) || !Set(AddAssign, PostIncr, PostDecr, SubAssign, MulAssign, DivAssign).contains(o.operator)) {
      super.visit(o)
      return
    } else if (updateStmnt.isDefined) {
      Fail("Multiple update statements for iteration variable %s. Only one update statement is allowed.", itervar)
    }

    o.operator match {
      //TODO OS add cases for i += -C (and for -=, *=, /=)
      case AddAssign =>
        updateStmnt = Some(Plus, o.second)
      case PostIncr =>
        updateStmnt = Some(Plus, create constant 1)
      case PostDecr =>
        updateStmnt = Some(Minus, create constant 1)
      case SubAssign =>
        updateStmnt = Some(Minus, o.second)
      case MulAssign =>
        updateStmnt = Some(Mult, o.second)
      case DivAssign =>
        updateStmnt = Some(Div, o.second)
      case _ =>
    }

    super.visit(o)
  }

  override def visit(s: AssignmentStatement): Unit = {
    if (!s.location.equals(itervar)) {
      super.visit(s)
      return
    } else if (updateStmnt.isDefined) {
      Fail("Multiple update statements for iteration variable %s. Only one update statement is allowed.", itervar)
    }

    s.expression match {
      case e: OperatorExpression => e.operator match {
        case Plus if e.first.equals(itervar) =>
          e.second match {
            case c: ConstantExpression =>
              updateStmnt = Some(Plus, e.second)
            case o: OperatorExpression if o.operator == UMinus && o.first.isInstanceOf[ConstantExpression] =>
              updateStmnt = Some(Minus, o.first)
          }
        case Plus if e.second.equals(itervar) =>
          e.first match {
            case c: ConstantExpression =>
              updateStmnt = Some(Plus, e.first)
            case o: OperatorExpression if o.operator == UMinus && o.first.isInstanceOf[ConstantExpression] =>
              updateStmnt = Some(Minus, o.first)
          }
        case Minus if e.first.equals(itervar) =>
          updateStmnt = Some(e.operator, e.second)
        //TODO OS this case should be purged everywhere. C - i is not a valid case.
        //        case Minus if e.second.equals(itervar) =>
//          updateStmnt = Some(e.operator, e.first)
        case FloorDiv if e.first.equals(itervar) =>
          updateStmnt = Some(e.operator, e.second)
        //        case (FloorDiv) if e.second.equals(itervar) =>
        //TODO OS should there be a warning here
        //          updateOp = e.operator
        //          updateConstant = e.first
        case Mult if e.first.equals(itervar) =>
          updateStmnt = Some(e.operator, e.second)
        case Mult if e.second.equals(itervar) =>
          updateStmnt = Some(e.operator, e.first)
        case _ => Warning("%s is not supported in the update statement at %s.", e.operator, e.getOrigin)
      }
      case _ =>
    }

    super.visit(s)
    // C is a ConstantExpression
    // -C is a OperatorExpression UMinus C
    //TODO OS treat -C + i as i - C

  }


}
