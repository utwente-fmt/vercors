package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrConcat, TSmtlibString, Type}
import vct.col.print._

trait SmtlibStrConcatImpl[G] { this: SmtlibStrConcat[G] =>
  override def t: Type[G] = TSmtlibString()
  // def layout(implicit ctx: Ctx): Doc = ???
}
