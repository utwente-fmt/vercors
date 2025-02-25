package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibRNA, TSmtlibRoundingMode, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibRNAOps

trait SmtlibRNAImpl[G] extends SmtlibRNAOps[G] {
  this: SmtlibRNA[G] =>
  override def t: Type[G] = TSmtlibRoundingMode()
  // def layout(implicit ctx: Ctx): Doc = ???
}
