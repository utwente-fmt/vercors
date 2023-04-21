package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpSqrt
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpSqrtImpl[G] { this: SmtlibFpSqrt[G] =>
  override def t: Type[G] = ???
  // def layout(implicit ctx: Ctx): Doc = ???
}
