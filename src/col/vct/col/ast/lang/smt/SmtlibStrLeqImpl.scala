package vct.col.ast.lang.smt

import vct.col.ast.SmtlibStrLeq
import vct.col.ast.Type
import vct.col.print._

trait SmtlibStrLeqImpl[G] { this: SmtlibStrLeq[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
