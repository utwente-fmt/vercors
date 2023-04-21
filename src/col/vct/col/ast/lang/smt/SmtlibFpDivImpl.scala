package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpDiv
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpDivImpl[G] { this: SmtlibFpDiv[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
