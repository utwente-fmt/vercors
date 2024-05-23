package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpFromSInt, TSmtlibFloatingPoint, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibFpFromSIntOps

trait SmtlibFpFromSIntImpl[G] extends SmtlibFpFromSIntOps[G] {
  this: SmtlibFpFromSInt[G] =>
  override def t: Type[G] =
    TSmtlibFloatingPoint(exponentBits, mantissaAndSignBits)
  // def layout(implicit ctx: Ctx): Doc = ???
}
