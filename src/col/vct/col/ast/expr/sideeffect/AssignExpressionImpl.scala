package vct.col.ast.expr.sideeffect

import vct.col.ast.expr.ExprImpl
import vct.col.ast.{AssignExpression, Expr, Local}
import vct.col.check.{CheckContext, CheckError}
import vct.col.origin.{AssignFailed, Blame}

trait AssignExpressionImpl[G] extends ExprImpl[G] {
  this: AssignExpression[G] =>
  def target: Expr[G]
  def value: Expr[G]
  def blame: Blame[AssignFailed]

  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context) ++
      (target match {
        case Local(ref) =>
          context.checkInWriteScope(context.roScopeReason, this, ref)
        case _ => Nil
      })
}
