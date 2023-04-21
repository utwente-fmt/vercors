package vct.col.ast.lang.smt

import vct.col.ast.SmtlibStrAt
import vct.col.ast.Type
import vct.col.print._

trait SmtlibStrAtImpl[G] { this: SmtlibStrAt[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
