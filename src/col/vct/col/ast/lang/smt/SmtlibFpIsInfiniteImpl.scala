package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpIsInfinite, TBool, Type}
import vct.col.print._

trait SmtlibFpIsInfiniteImpl[G] {
  this: SmtlibFpIsInfinite[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
