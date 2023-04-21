package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBvNeg
import vct.col.ast.Type
import vct.col.print._

trait SmtlibBvNegImpl[G] { this: SmtlibBvNeg[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
