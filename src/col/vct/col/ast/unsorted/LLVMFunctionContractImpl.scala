package vct.col.ast.unsorted

import vct.col.ast.LLVMFunctionContract
import vct.col.ast.ops.LLVMFunctionContractFamilyOps
import vct.col.print._

trait LLVMFunctionContractImpl[G] extends LLVMFunctionContractFamilyOps[G] {
  this: LLVMFunctionContract[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
