package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpEq
import vct.col.ast.{Type, TBool}
import vct.col.print._

trait SmtlibFpEqImpl[G] {
  this: SmtlibFpEq[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
