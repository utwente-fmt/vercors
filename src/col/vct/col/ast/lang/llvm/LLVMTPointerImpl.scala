package vct.col.ast.lang.llvm

import vct.col.ast.LLVMTPointer
import vct.col.ast.ops.LLVMTPointerOps

trait LLVMTPointerImpl[G] extends LLVMTPointerOps[G] {
  this: LLVMTPointer[G] =>

}
