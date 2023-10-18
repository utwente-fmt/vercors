package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpMax
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpMaxImpl[G] {
  this: SmtlibFpMax[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
