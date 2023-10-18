package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpMin
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpMinImpl[G] {
  this: SmtlibFpMin[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
