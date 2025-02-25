package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibRNE, TSmtlibRoundingMode, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibRNEOps

trait SmtlibRNEImpl[G] extends SmtlibRNEOps[G] {
  this: SmtlibRNE[G] =>
  override def t: Type[G] = TSmtlibRoundingMode()
  // def layout(implicit ctx: Ctx): Doc = ???
}
