package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrLt, TBool, Type}
import vct.col.print._

trait SmtlibStrLtImpl[G] {
  this: SmtlibStrLt[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
