package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpLeq
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpLeqImpl[G] { this: SmtlibFpLeq[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
