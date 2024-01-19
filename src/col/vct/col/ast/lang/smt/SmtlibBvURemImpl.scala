package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBvURem
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibBvURemOps

trait SmtlibBvURemImpl[G] extends SmtlibBvURemOps[G] { this: SmtlibBvURem[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
