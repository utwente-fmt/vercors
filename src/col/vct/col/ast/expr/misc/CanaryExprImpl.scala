package vct.col.ast.expr.misc

import vct.col.ast.ops.CanaryExprOps
import vct.col.ast.{CanaryExpr, TNothing, Type}

trait CanaryExprImpl[G] extends CanaryExprOps[G] {
  this: CanaryExpr[G] =>

  override def t: Type[G] = TNothing()
}
