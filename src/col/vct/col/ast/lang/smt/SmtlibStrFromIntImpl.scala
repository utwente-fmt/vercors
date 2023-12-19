package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrFromInt, TSmtlibString, Type}
import vct.col.print._

trait SmtlibStrFromIntImpl[G] { this: SmtlibStrFromInt[G] =>
  override def t: Type[G] = TSmtlibString()
  // def layout(implicit ctx: Ctx): Doc = ???
}
