package vct.col.ast.expr.op

import vct.col.ast.{BinExpr, Expr}

trait BinExprImpl[G] { this: BinExpr[G] =>
  def left: Expr[G]
  def right: Expr[G]
}