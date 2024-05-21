package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpRem
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibFpRemOps

trait SmtlibFpRemImpl[G] extends SmtlibFpRemOps[G] { this: SmtlibFpRem[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
