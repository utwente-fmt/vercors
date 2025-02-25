package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpFma
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibFpFmaOps

trait SmtlibFpFmaImpl[G] extends SmtlibFpFmaOps[G] {
  this: SmtlibFpFma[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
