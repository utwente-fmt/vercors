package vct.col.ast.lang.llvm

import vct.col.ast.LLVMMemoryAcquireRelease
import vct.col.ast.ops.LLVMMemoryAcquireReleaseOps
import vct.col.print._

trait LLVMMemoryAcquireReleaseImpl[G] extends LLVMMemoryAcquireReleaseOps[G] {
  this: LLVMMemoryAcquireRelease[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
