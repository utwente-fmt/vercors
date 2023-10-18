package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReRange, TSmtlibRegLan, Type}
import vct.col.print._

trait SmtlibReRangeImpl[G] {
  this: SmtlibReRange[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
