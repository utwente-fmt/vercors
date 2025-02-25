package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpFromReal, TSmtlibFloatingPoint, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibFpFromRealOps

trait SmtlibFpFromRealImpl[G] extends SmtlibFpFromRealOps[G] {
  this: SmtlibFpFromReal[G] =>
  override def t: Type[G] =
    TSmtlibFloatingPoint(exponentBits, mantissaAndSignBits)
  // def layout(implicit ctx: Ctx): Doc = ???
}
