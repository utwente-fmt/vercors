package vct.col.ast.lang.smt

import vct.col.ast.SmtlibSelect
import vct.col.ast.Type
import vct.col.print._

trait SmtlibSelectImpl[G] { this: SmtlibSelect[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
