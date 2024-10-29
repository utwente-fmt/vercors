package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpSub
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibFpSubOps

trait SmtlibFpSubImpl[G] extends SmtlibFpSubOps[G] {
  this: SmtlibFpSub[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
