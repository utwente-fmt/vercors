package vct.col.ast.lang.smt

import vct.col.ast.{SmtlibFpLeq, TBool, Type}
import vct.col.print._

trait SmtlibFpLeqImpl[G] { this: SmtlibFpLeq[G] =>
  override def t: Type[G] = TBool()
  // def layout(implicit ctx: Ctx): Doc = ???
}
