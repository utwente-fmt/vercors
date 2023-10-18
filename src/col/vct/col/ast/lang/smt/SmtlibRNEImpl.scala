package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibRNE, TSmtlibRoundingMode, Type}
import vct.col.print._

trait SmtlibRNEImpl[G] {
  this: SmtlibRNE[G] =>
  override def t: Type[G] = TSmtlibRoundingMode()
  // def layout(implicit ctx: Ctx): Doc = ???
}
