package vct.col.ast.temporaryimplpackage.expr.op

import vct.col.ast.{Expr, UnExpr}

trait UnExprImpl { this: UnExpr =>
  def arg: Expr
}