package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBvNot
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibBvNotOps

trait SmtlibBvNotImpl[G] extends SmtlibBvNotOps[G] { this: SmtlibBvNot[G] =>
  override def t: Type[G] = bv.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
