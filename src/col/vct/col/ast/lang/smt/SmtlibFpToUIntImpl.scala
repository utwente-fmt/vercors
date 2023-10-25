package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpToUInt, TSmtlibBitVector, Type}
import vct.col.print._

trait SmtlibFpToUIntImpl[G] { this: SmtlibFpToUInt[G] =>
  override def t: Type[G] = TSmtlibBitVector(bits)
  // def layout(implicit ctx: Ctx): Doc = ???
}
