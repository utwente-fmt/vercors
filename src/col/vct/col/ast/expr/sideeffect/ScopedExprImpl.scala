package vct.col.ast.expr.sideeffect

import vct.col.ast.{ScopedExpr, Type}
import vct.col.ast.ops.ScopedExprOps

trait ScopedExprImpl[G] extends ScopedExprOps[G] {
  this: ScopedExpr[G] =>
  override def t: Type[G] = body.t
}
