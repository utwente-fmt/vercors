package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpIsZero, TBool, Type}
import vct.col.print._

trait SmtlibFpIsZeroImpl[G] {
  this: SmtlibFpIsZero[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
