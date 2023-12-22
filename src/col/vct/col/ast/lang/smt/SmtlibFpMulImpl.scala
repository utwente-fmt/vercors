package vct.col.ast.lang.smt

import vct.col.ast.SmtlibFpMul
import vct.col.ast.Type
import vct.col.print._
import vct.col.ast.ops.SmtlibFpMulOps

trait SmtlibFpMulImpl[G] extends SmtlibFpMulOps[G] { this: SmtlibFpMul[G] =>
  override def t: Type[G] = left.t
  // def layout(implicit ctx: Ctx): Doc = ???
}
