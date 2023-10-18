package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpCast, TSmtlibFloatingPoint, Type}
import vct.col.print._

trait SmtlibFpCastImpl[G] {
  this: SmtlibFpCast[G] =>
  override def t: Type[G] =
    TSmtlibFloatingPoint(exponentBits, mantissaAndSignBits)
  // def layout(implicit ctx: Ctx): Doc = ???
}
