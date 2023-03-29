package vct.col.ast.expr.sideeffect

import vct.col.ast.{PostAssignExpression, Type}

trait PostAssignExpressionImpl[G] { this: PostAssignExpression[G] =>
  override def t: Type[G] = target.t
}