package vct.col.ast.lang.llvm

import vct.col.ast.LLVMMemorySequentiallyConsistent
import vct.col.ast.ops.LLVMMemorySequentiallyConsistentOps
import vct.col.print._

trait LLVMMemorySequentiallyConsistentImpl[G]
    extends LLVMMemorySequentiallyConsistentOps[G] {
  this: LLVMMemorySequentiallyConsistent[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
