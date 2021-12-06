package vct.col.ast.temporaryimplpackage.expr.sideeffect

import vct.col.ast.{AssignExpression, Expr}

trait AssignExpressionImpl { this: AssignExpression =>
  def target: Expr
  def value: Expr
}