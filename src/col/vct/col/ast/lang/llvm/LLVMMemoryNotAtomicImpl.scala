package vct.col.ast.lang.llvm

import vct.col.ast.LLVMMemoryNotAtomic
import vct.col.ast.ops.LLVMMemoryNotAtomicOps

trait LLVMMemoryNotAtomicImpl[G] extends LLVMMemoryNotAtomicOps[G] {
  this: LLVMMemoryNotAtomic[G] =>
}
