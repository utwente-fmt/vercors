package vct.col.ast.temporaryimplpackage.expr.sideeffect

import vct.col.ast.{PreAssignExpression, Type}

trait PreAssignExpressionImpl[G] { this: PreAssignExpression[G] =>
  override def t: Type[G] = value.t
}