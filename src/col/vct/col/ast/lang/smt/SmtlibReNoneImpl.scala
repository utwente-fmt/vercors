package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReNone, TSmtlibRegLan, Type}
import vct.col.print._

trait SmtlibReNoneImpl[G] {
  this: SmtlibReNone[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
