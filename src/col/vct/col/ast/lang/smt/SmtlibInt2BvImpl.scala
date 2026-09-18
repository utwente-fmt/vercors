package vct.col.ast.lang.smt

import vct.col.ast.ops.SmtlibInt2BvOps
import vct.col.ast.{SmtlibInt2Bv, TSmtlibBitVector, Type}

trait SmtlibInt2BvImpl[G] extends SmtlibInt2BvOps[G] {
  this: SmtlibInt2Bv[G] =>
  override def t: Type[G] = TSmtlibBitVector(size)
  // def layout(implicit ctx: Ctx): Doc = ???
}
