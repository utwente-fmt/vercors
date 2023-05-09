package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrFromCode, TSmtlibString, Type}
import vct.col.print._

trait SmtlibStrFromCodeImpl[G] { this: SmtlibStrFromCode[G] =>
  override def t: Type[G] = TSmtlibString()
  // def layout(implicit ctx: Ctx): Doc = ???
}
