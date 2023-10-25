package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrReplace, TSmtlibString, Type}
import vct.col.print._

trait SmtlibStrReplaceImpl[G] { this: SmtlibStrReplace[G] =>
  override def t: Type[G] = TSmtlibString()
  // def layout(implicit ctx: Ctx): Doc = ???
}
