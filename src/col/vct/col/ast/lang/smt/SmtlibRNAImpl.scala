package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibRNA, TSmtlibRoundingMode, Type}
import vct.col.print._

trait SmtlibRNAImpl[G] {
  this: SmtlibRNA[G] =>
  override def t: Type[G] = TSmtlibRoundingMode()
  // def layout(implicit ctx: Ctx): Doc = ???
}
