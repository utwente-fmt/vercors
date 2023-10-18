package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibStrLeq, TBool, Type}
import vct.col.print._

trait SmtlibStrLeqImpl[G] {
  this: SmtlibStrLeq[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
