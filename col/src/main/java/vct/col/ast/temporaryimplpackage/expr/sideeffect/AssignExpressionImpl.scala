package vct.col.ast.temporaryimplpackage.expr.sideeffect

import vct.col.ast.{AssignExpression, Expr}

trait AssignExpressionImpl[G] { this: AssignExpression[G] =>
  def target: Expr[G]
  def value: Expr[G]
}