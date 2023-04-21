package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBvShr
import vct.col.ast.Type
import vct.col.print._

trait SmtlibBvShrImpl[G] { this: SmtlibBvShr[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
