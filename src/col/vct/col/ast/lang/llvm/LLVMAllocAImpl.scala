package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMAllocAOps
import vct.col.ast.{LLVMAllocA, Type, LLVMTPointer}

trait LLVMAllocAImpl[G] extends LLVMAllocAOps[G] {
  this: LLVMAllocA[G] =>
}
