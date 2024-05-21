package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpSqrt
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibFpSqrtOps

trait SmtlibFpSqrtImpl[G] extends SmtlibFpSqrtOps[G] { this: SmtlibFpSqrt[G] =>
  override def t: Type[G] = arg.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
