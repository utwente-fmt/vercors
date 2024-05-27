package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpToSInt, TSmtlibBitVector, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibFpToSIntOps

trait SmtlibFpToSIntImpl[G] extends SmtlibFpToSIntOps[G] {
  this: SmtlibFpToSInt[G] =>
  override def t: Type[G] = TSmtlibBitVector(bits)
  // def layout(implicit ctx: Ctx): Doc = ???
}
