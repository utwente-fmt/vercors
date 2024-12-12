package vct.col.ast.lang.llvm

import vct.col.ast.F16
import vct.col.ast.ops.F16Ops
import vct.col.print._

trait F16Impl[G] extends F16Ops[G] {
  this: F16[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("f16")
}
