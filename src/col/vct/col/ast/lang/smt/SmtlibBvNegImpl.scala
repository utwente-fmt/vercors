package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBvNeg
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibBvNegOps

trait SmtlibBvNegImpl[G] extends SmtlibBvNegOps[G] { this: SmtlibBvNeg[G] =>
  override def t: Type[G] = bv.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
