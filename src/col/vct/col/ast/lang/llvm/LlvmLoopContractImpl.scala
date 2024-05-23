package vct.col.ast.lang.llvm

import vct.col.ast.LlvmLoopContract
import vct.col.ast.ops.LlvmLoopContractFamilyOps

trait LlvmLoopContractImpl[G] extends LlvmLoopContractFamilyOps[G] {
  this: LlvmLoopContract[G] =>

}
