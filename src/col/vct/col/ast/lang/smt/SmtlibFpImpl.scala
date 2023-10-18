package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFp, TSmtlibFloatingPoint, Type}
import vct.col.print._

trait SmtlibFpImpl[G] {
  this: SmtlibFp[G] =>
  override def t: Type[G] =
    TSmtlibFloatingPoint(
      exponent.t.asBitvec.get.size,
      mantissa.t.asBitvec.get.size + 1,
    )
  // def layout(implicit ctx: Ctx): Doc = ???
}
