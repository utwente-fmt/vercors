package vct.col.ast.temporaryimplpackage.expr.sideeffect

import vct.col.ast.{PreAssignExpression, Type}

trait PreAssignExpressionImpl { this: PreAssignExpression =>
  override def t: Type = value.t
}