package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibBitvecLiteral, TSmtlibBitVector, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibBitvecLiteralOps

trait SmtlibBitvecLiteralImpl[G] extends SmtlibBitvecLiteralOps[G] {
  this: SmtlibBitvecLiteral[G] =>
  override def t: Type[G] = TSmtlibBitVector(data.length)
  // def layout(implicit ctx: Ctx): Doc = ???
}
