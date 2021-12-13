package vct.col.ast.temporaryimplpackage.expr.op

import vct.col.ast.{Expr, UnExpr}

trait UnExprImpl[G] { this: UnExpr[G] =>
  def arg: Expr[G]
}