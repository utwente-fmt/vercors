package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibRTZ, TSmtlibRoundingMode, Type}
import vct.col.print._

trait SmtlibRTZImpl[G] {
  this: SmtlibRTZ[G] =>
  override def t: Type[G] = TSmtlibRoundingMode()
  // def layout(implicit ctx: Ctx): Doc = ???
}
