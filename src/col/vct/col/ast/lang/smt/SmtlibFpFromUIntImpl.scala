package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpFromUInt, TSmtlibFloatingPoint, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibFpFromUIntOps

trait SmtlibFpFromUIntImpl[G] extends SmtlibFpFromUIntOps[G] {
  this: SmtlibFpFromUInt[G] =>
  override def t: Type[G] =
    TSmtlibFloatingPoint(exponentBits, mantissaAndSignBits)
  // def layout(implicit ctx: Ctx): Doc = ???
}
