package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReFromStr, TSmtlibRegLan, Type}
import vct.col.print._

trait SmtlibReFromStrImpl[G] {
  this: SmtlibReFromStr[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
