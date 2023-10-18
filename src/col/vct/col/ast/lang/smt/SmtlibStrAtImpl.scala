package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrAt, TSmtlibString, Type}
import vct.col.print._

trait SmtlibStrAtImpl[G] {
  this: SmtlibStrAt[G] =>
  override def t: Type[G] = TSmtlibString()
  // def layout(implicit ctx: Ctx): Doc = ???
}
