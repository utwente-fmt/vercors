package vct.col.ast.lang.smt

import vct.col.ast.SmtlibStrLt
import vct.col.ast.Type
import vct.col.print._

trait SmtlibStrLtImpl[G] { this: SmtlibStrLt[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
