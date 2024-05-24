package vct.col.ast.lang.llvm

import vct.col.ast.LlvmFunctionContract
import vct.col.ast.ops.{LlvmFunctionContractOps, LlvmFunctionContractFamilyOps}

trait LlvmFunctionContractImpl[G] extends LlvmFunctionContractOps[G] with LlvmFunctionContractFamilyOps[G] { this: LlvmFunctionContract[G] =>
}
