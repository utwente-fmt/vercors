package vct.col.ast.lang.llvm

import vct.col.ast.LLVMMemoryMonotonic
import vct.col.ast.ops.LLVMMemoryMonotonicOps
import vct.col.print._

trait LLVMMemoryMonotonicImpl[G] extends LLVMMemoryMonotonicOps[G] {
  this: LLVMMemoryMonotonic[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
