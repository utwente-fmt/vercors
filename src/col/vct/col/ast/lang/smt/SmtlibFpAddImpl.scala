package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpAdd
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibFpAddOps

trait SmtlibFpAddImpl[G] extends SmtlibFpAddOps[G] {
  this: SmtlibFpAdd[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
