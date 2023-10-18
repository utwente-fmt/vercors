package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBvOr
import vct.col.ast.Type
import vct.col.print._

trait SmtlibBvOrImpl[G] {
  this: SmtlibBvOr[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
