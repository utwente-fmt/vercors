package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibConcat, TSmtlibBitVector, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibConcatOps

trait SmtlibConcatImpl[G] extends SmtlibConcatOps[G] {
  this: SmtlibConcat[G] =>
  override lazy val t: Type[G] = TSmtlibBitVector(
    left.t.asBitvec.get.size + right.t.asBitvec.get.size
  )
  // def layout(implicit ctx: Ctx): Doc = ???
}
