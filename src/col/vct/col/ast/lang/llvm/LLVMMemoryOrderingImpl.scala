package vct.col.ast.lang.llvm

import vct.col.ast.LLVMMemoryOrdering
import vct.col.ast.ops.LLVMMemoryOrderingFamilyOps

trait LLVMMemoryOrderingImpl[G] extends LLVMMemoryOrderingFamilyOps[G] {
  this: LLVMMemoryOrdering[G] =>
}
