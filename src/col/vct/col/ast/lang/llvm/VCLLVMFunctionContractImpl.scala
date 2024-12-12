package vct.col.ast.lang.llvm

import vct.col.ast.VCLLVMFunctionContract
import vct.col.ast.ops.{VCLLVMFunctionContractOps}

trait VCLLVMFunctionContractImpl[G] extends VCLLVMFunctionContractOps[G] {
  this: VCLLVMFunctionContract[G] =>
}
