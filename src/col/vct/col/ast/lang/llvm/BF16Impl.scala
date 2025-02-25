package vct.col.ast.lang.llvm

import vct.col.ast.BF16
import vct.col.ast.ops.BF16Ops
import vct.col.print._

trait BF16Impl[G] extends BF16Ops[G] {
  this: BF16[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("bf16")
}
