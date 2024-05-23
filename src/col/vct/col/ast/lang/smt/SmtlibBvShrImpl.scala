package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBvShr
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibBvShrOps

trait SmtlibBvShrImpl[G] extends SmtlibBvShrOps[G] { this: SmtlibBvShr[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
