package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibRTN, TSmtlibRoundingMode, Type}
import vct.col.print._

trait SmtlibRTNImpl[G] {
  this: SmtlibRTN[G] =>
  override def t: Type[G] = TSmtlibRoundingMode()
  // def layout(implicit ctx: Ctx): Doc = ???
}
