package vct.col.ast.temporaryimplpackage.expr.op

import vct.col.ast.{BinExpr, Expr}

trait BinExprImpl { this: BinExpr =>
  def left: Expr
  def right: Expr
}