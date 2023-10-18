package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReRepeat, TSmtlibRegLan, Type}
import vct.col.print._

trait SmtlibReRepeatImpl[G] {
  this: SmtlibReRepeat[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
