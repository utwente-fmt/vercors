package vct.col.ast.lang.llvm

import vct.col.ast.F128
import vct.col.ast.ops.F128Ops
import vct.col.print._

trait F128Impl[G] extends F128Ops[G] {
  this: F128[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("f128")
}
