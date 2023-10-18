package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrReplaceAll, TSmtlibString, Type}
import vct.col.print._

trait SmtlibStrReplaceAllImpl[G] {
  this: SmtlibStrReplaceAll[G] =>
  override def t: Type[G] = TSmtlibString()
  // def layout(implicit ctx: Ctx): Doc = ???
}
