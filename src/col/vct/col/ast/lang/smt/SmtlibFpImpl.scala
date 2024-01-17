package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFp, TSmtlibFloatingPoint, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibFpOps

trait SmtlibFpImpl[G] extends SmtlibFpOps[G] { this: SmtlibFp[G] =>
  override def t: Type[G] = TSmtlibFloatingPoint(exponent.t.asBitvec.get.size, mantissa.t.asBitvec.get.size + 1)
  // def layout(implicit ctx: Ctx): Doc = ???
}
