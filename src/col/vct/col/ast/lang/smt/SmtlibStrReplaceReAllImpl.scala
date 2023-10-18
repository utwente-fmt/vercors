package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrReplaceReAll, TSmtlibString, Type}
import vct.col.print._

trait SmtlibStrReplaceReAllImpl[G] {
  this: SmtlibStrReplaceReAll[G] =>
  override def t: Type[G] = TSmtlibString()
  // def layout(implicit ctx: Ctx): Doc = ???
}
