package vct.col.ast.unsorted

import vct.col.ast.{AssumeExpr, Type}
import vct.col.ast.ops.AssumeExprOps
import vct.col.print._

trait AssumeExprImpl[G] extends AssumeExprOps[G] {
  this: AssumeExpr[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
  override def t: Type[G] = inner.t
}
