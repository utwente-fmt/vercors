package vct.col.ast.lang.llvm

import vct.col.ast.LLVMMemoryNotAtomic
import vct.col.ast.ops.LLVMMemoryNotAtomicOps
import vct.col.print._

trait LLVMMemoryNotAtomicImpl[G] extends LLVMMemoryNotAtomicOps[G] {
  this: LLVMMemoryNotAtomic[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
