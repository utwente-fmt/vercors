package vct.col.ast.expr.op.num

import vct.col.ast.DividingExpr
import vct.col.origin.{Blame, DivByZero}

trait DividingExprImpl[G] {
  this: DividingExpr[G] =>
  def blame: Blame[DivByZero]
}
