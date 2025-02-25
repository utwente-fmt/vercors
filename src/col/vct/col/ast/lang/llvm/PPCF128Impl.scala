package vct.col.ast.lang.llvm

import vct.col.ast.PPCF128
import vct.col.ast.ops.PPCF128Ops
import vct.col.print._

trait PPCF128Impl[G] extends PPCF128Ops[G] {
  this: PPCF128[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("ppc_f128")
}
