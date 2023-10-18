package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpGeq, TBool, Type}
import vct.col.print._

trait SmtlibFpGeqImpl[G] {
  this: SmtlibFpGeq[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
