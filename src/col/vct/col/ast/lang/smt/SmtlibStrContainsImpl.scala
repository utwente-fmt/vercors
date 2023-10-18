package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrContains, TBool, Type}
import vct.col.print._

trait SmtlibStrContainsImpl[G] {
  this: SmtlibStrContains[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
