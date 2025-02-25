package vct.col.ast.lang.smt

import vct.col.ast.TSmtlibBitVector
import vct.col.ast.ops.TSmtlibBitVectorOps
import vct.col.print._

trait TSmtlibBitVectorImpl[G] extends TSmtlibBitVectorOps[G] {
  this: TSmtlibBitVector[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
