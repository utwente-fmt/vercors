package vct.col.ast.temporaryimplpackage.expr.sideeffect

import vct.col.ast.{PostAssignExpression, Type}

trait PostAssignExpressionImpl { this: PostAssignExpression =>
  override def t: Type = target.t
}