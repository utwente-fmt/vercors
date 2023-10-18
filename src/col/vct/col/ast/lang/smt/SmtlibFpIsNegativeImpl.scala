package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpIsNegative, TBool, Type}
import vct.col.print._

trait SmtlibFpIsNegativeImpl[G] {
  this: SmtlibFpIsNegative[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
