package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpGt
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpGtImpl[G] { this: SmtlibFpGt[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
