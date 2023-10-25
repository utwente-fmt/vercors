package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpFromSInt, TSmtlibFloatingPoint, Type}
import vct.col.print._

trait SmtlibFpFromSIntImpl[G] { this: SmtlibFpFromSInt[G] =>
  override def t: Type[G] = TSmtlibFloatingPoint(exponentBits, mantissaAndSignBits)
  // def layout(implicit ctx: Ctx): Doc = ???
}
