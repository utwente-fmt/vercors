package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBvULt
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibBvULtOps

trait SmtlibBvULtImpl[G] extends SmtlibBvULtOps[G] {
  this: SmtlibBvULt[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
