package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBvUDiv
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibBvUDivOps

trait SmtlibBvUDivImpl[G] extends SmtlibBvUDivOps[G] {
  this: SmtlibBvUDiv[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
