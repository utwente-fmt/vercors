package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibRTP, TSmtlibRoundingMode, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibRTPOps

trait SmtlibRTPImpl[G] extends SmtlibRTPOps[G] {
  this: SmtlibRTP[G] =>
  override def t: Type[G] = TSmtlibRoundingMode()
  // def layout(implicit ctx: Ctx): Doc = ???
}
