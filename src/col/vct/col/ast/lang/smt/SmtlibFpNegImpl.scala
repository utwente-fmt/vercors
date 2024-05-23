package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpNeg
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibFpNegOps

trait SmtlibFpNegImpl[G] extends SmtlibFpNegOps[G] {
  this: SmtlibFpNeg[G] =>
  override def t: Type[G] = arg.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
