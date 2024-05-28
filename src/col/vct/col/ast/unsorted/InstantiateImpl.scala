package vct.col.ast.unsorted

import vct.col.ast.{Expr, Instantiate}
import vct.col.ast.ops.InstantiateOps
import vct.col.print._

trait InstantiateImpl[G] extends InstantiateOps[G] {
  this: Instantiate[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
  override def expr: Expr[G] = this.out
}
