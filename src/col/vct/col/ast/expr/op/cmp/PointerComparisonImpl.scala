package vct.col.ast.expr.op.cmp

import vct.col.ast.{Expr, PointerComparison}

trait PointerComparisonImpl[G] {
  this: PointerComparison[G] =>

  def elementSize: Expr[G]
}
