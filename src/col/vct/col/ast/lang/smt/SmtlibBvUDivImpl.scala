package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBvUDiv
import vct.col.ast.Type
import vct.col.print._

trait SmtlibBvUDivImpl[G] { this: SmtlibBvUDiv[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
