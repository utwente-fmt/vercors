package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBvAnd
import vct.col.ast.Type
import vct.col.print._

trait SmtlibBvAndImpl[G] { this: SmtlibBvAnd[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
