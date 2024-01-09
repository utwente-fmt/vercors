package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpRoundToIntegral
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibFpRoundToIntegralOps

trait SmtlibFpRoundToIntegralImpl[G] extends SmtlibFpRoundToIntegralOps[G] { this: SmtlibFpRoundToIntegral[G] =>
  override def t: Type[G] = arg.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
