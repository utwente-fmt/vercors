package vct.col.ast.lang.llvm

import vct.col.ast.LLVMFunctionContract
import vct.col.ast.ops.{LLVMFunctionContractOps, LLVMFunctionContractFamilyOps}

trait LLVMFunctionContractImpl[G]
    extends LLVMFunctionContractOps[G] with LLVMFunctionContractFamilyOps[G] {
  this: LLVMFunctionContract[G] =>
}
