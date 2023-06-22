package vct.col.ast.expr.sideeffect

import vct.col.ast.expr.ExprImpl
import vct.col.ast.{AssignExpression, Expr, Local}
import vct.col.check.{CheckContext, CheckMessage}

trait AssignExpressionImpl[G] extends ExprImpl[G] { this: AssignExpression[G] =>
  def target: Expr[G]
  def value: Expr[G]

  override def check(context: CheckContext[G]): Seq[CheckMessage] =
    super.check(context) ++ (target match {
      case Local(ref) => context.checkInWriteScope(context.roScopeReason, this, ref)
      case _ => Nil
    })
}