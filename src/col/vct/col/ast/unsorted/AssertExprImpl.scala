package vct.col.ast.unsorted

import vct.col.ast.{AssertExpr, Type}
import vct.col.ast.ops.AssertExprOps
import vct.col.print._

trait AssertExprImpl[G] extends AssertExprOps[G] {
  this: AssertExpr[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
  override def t: Type[G] = inner.t
}
