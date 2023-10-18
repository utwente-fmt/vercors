package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibSubstr, TSmtlibString, Type}
import vct.col.print._

trait SmtlibSubstrImpl[G] {
  this: SmtlibSubstr[G] =>
  override def t: Type[G] = TSmtlibString()
  // def layout(implicit ctx: Ctx): Doc = ???
}
