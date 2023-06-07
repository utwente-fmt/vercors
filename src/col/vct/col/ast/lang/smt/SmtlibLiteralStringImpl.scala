package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibLiteralString, TSmtlibString, Type}
import vct.col.print._

trait SmtlibLiteralStringImpl[G] { this: SmtlibLiteralString[G] =>
  override def t: Type[G] = TSmtlibString()
  // def layout(implicit ctx: Ctx): Doc = ???
}
