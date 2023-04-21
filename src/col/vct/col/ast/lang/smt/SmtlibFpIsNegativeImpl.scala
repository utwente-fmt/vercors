package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpIsNegative
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpIsNegativeImpl[G] { this: SmtlibFpIsNegative[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
