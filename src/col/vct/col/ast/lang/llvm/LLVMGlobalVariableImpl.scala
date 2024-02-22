package vct.col.ast.lang.llvm

import vct.col.ast.LLVMGlobalVariable
import vct.col.ast.ops.LLVMGlobalVariableOps
import vct.col.print._

trait LLVMGlobalVariableImpl[G] extends LLVMGlobalVariableOps[G] {
  this: LLVMGlobalVariable[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
