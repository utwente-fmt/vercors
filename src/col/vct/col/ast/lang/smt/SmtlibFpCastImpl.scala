package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpCast, TSmtlibFloatingPoint, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibFpCastOps

trait SmtlibFpCastImpl[G] extends SmtlibFpCastOps[G] { this: SmtlibFpCast[G] =>
  override def t: Type[G] = TSmtlibFloatingPoint(exponentBits, mantissaAndSignBits)
  // def layout(implicit ctx: Ctx): Doc = ???
}
