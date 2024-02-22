package vct.col.ast.lang.llvm

import vct.col.ast.LLVMStore
import vct.col.ast.ops.LLVMStoreOps

trait LLVMStoreImpl[G] extends LLVMStoreOps[G] {
  this: LLVMStore[G] =>
}
