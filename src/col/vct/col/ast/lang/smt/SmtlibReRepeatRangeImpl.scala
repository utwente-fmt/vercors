package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibReRepeatRange, TSmtlibRegLan, Type}
import vct.col.print._

trait SmtlibReRepeatRangeImpl[G] { this: SmtlibReRepeatRange[G] =>
  override def t: Type[G] = TSmtlibRegLan()
  // def layout(implicit ctx: Ctx): Doc = ???
}
