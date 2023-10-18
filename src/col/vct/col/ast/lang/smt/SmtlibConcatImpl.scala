package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibConcat, TSmtlibBitVector, Type}
import vct.col.print._

trait SmtlibConcatImpl[G] {
  this: SmtlibConcat[G] =>
  override lazy val t: Type[G] = TSmtlibBitVector(
    left.t.asBitvec.get.size + right.t.asBitvec.get.size
  )
  // def layout(implicit ctx: Ctx): Doc = ???
}
