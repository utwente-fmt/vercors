package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpLt
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpLtImpl[G] { this: SmtlibFpLt[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
