package vct.col.ast.lang.llvm

import vct.col.ast.LLVMMemoryAcquire
import vct.col.ast.ops.LLVMMemoryAcquireOps
import vct.col.print._

trait LLVMMemoryAcquireImpl[G] extends LLVMMemoryAcquireOps[G] {
  this: LLVMMemoryAcquire[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("acquire")
}
