package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpDiv
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibFpDivOps

trait SmtlibFpDivImpl[G] extends SmtlibFpDivOps[G] {
  this: SmtlibFpDiv[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
