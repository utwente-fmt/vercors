package vct.col.ast.lang.llvm

import vct.col.ast.LLVMLoopContract
import vct.col.ast.ops.LLVMLoopContractFamilyOps

trait LLVMLoopContractImpl[G] extends LLVMLoopContractFamilyOps[G] {
  this: LLVMLoopContract[G] =>

}
