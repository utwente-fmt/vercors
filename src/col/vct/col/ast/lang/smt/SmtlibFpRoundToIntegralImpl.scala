package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpRoundToIntegral
import vct.col.ast.Type
import vct.col.print._

trait SmtlibFpRoundToIntegralImpl[G] {
  this: SmtlibFpRoundToIntegral[G] =>
  override def t: Type[G] = arg.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
