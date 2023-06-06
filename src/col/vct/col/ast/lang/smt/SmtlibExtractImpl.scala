package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibExtract, TSmtlibBitVector, Type}
import vct.col.print._

trait SmtlibExtractImpl[G] { this: SmtlibExtract[G] =>
  override def t: Type[G] = TSmtlibBitVector(inclusiveEndIndexFromRight - startIndexFromRight + 1)
  // def layout(implicit ctx: Ctx): Doc = ???
}
