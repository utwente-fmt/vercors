package vct.col.ast.lang.smt

import vct.col.ast.SmtlibStrContains
import vct.col.ast.Type
import vct.col.print._

trait SmtlibStrContainsImpl[G] { this: SmtlibStrContains[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
