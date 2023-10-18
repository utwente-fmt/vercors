package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpToSInt, TSmtlibBitVector, Type}
import vct.col.print._

trait SmtlibFpToSIntImpl[G] {
  this: SmtlibFpToSInt[G] =>
  override def t: Type[G] = TSmtlibBitVector(bits)
  // def layout(implicit ctx: Ctx): Doc = ???
}
