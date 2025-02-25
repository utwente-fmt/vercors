package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBvAnd
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibBvAndOps

trait SmtlibBvAndImpl[G] extends SmtlibBvAndOps[G] {
  this: SmtlibBvAnd[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
