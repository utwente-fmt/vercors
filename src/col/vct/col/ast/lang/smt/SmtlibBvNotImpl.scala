package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBvNot
import vct.col.ast.Type
import vct.col.print._

trait SmtlibBvNotImpl[G] { this: SmtlibBvNot[G] =>
  override def t: Type[G] = bv.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
