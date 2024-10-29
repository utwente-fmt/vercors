package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibExtract, TSmtlibBitVector, Type}
import vct.col.print._
import vct.col.ast.ops.SmtlibExtractOps

trait SmtlibExtractImpl[G] extends SmtlibExtractOps[G] {
  this: SmtlibExtract[G] =>
  override def t: Type[G] =
    TSmtlibBitVector(inclusiveEndIndexFromRight - startIndexFromRight + 1)
  // def layout(implicit ctx: Ctx): Doc = ???
}
