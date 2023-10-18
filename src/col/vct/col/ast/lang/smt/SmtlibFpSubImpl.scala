package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpSub
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpSubImpl[G] {
  this: SmtlibFpSub[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
