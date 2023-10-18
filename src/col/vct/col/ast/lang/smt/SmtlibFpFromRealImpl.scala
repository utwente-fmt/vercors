package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpFromReal, TSmtlibFloatingPoint, Type}
import vct.col.print._

trait SmtlibFpFromRealImpl[G] {
  this: SmtlibFpFromReal[G] =>
  override def t: Type[G] =
    TSmtlibFloatingPoint(exponentBits, mantissaAndSignBits)
  // def layout(implicit ctx: Ctx): Doc = ???
}
