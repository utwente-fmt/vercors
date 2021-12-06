package vct.col.ast.temporaryimplpackage.expr.op.num

import vct.col.ast.DividingExpr
import vct.col.origin.{Blame, DivByZero}

trait DividingExprImpl { this: DividingExpr =>
  def blame: Blame[DivByZero]
}