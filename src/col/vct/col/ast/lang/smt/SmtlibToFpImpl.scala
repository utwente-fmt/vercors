package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibToFp, TSmtlibFloatingPoint, Type}
import vct.col.print._

trait SmtlibToFpImpl[G] {
  this: SmtlibToFp[G] =>
  override def t: Type[G] =
    TSmtlibFloatingPoint(exponentBits, mantissaAndSignBits)
  // def layout(implicit ctx: Ctx): Doc = ???
}
