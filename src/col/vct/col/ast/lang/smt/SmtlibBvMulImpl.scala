package vct.col.ast.lang.smt

import vct.col.ast.SmtlibBvMul
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibBvMulOps

trait SmtlibBvMulImpl[G] extends SmtlibBvMulOps[G] { this: SmtlibBvMul[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
