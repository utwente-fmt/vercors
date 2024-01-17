package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpAbs
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibFpAbsOps

trait SmtlibFpAbsImpl[G] extends SmtlibFpAbsOps[G] { this: SmtlibFpAbs[G] =>
  override def t: Type[G] = arg.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
