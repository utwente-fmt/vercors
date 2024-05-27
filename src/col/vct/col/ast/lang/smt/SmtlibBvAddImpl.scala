package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBvAdd
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibBvAddOps

trait SmtlibBvAddImpl[G] extends SmtlibBvAddOps[G] {
  this: SmtlibBvAdd[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
