package vct.col.ast.expr.op.num

import vct.col.ast.{Blame1, DividingExpr}
import vct.col.origin.{Blame, DivByZero}

trait DividingExprImpl[G] { this: DividingExpr[G] =>
  def blame: Blame1[G]
}