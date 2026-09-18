package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMBranchUnreachableOps
import vct.col.ast.LLVMBranchUnreachable
import vct.col.print._

trait LLVMBranchUnreachableImpl[G] extends LLVMBranchUnreachableOps[G] {
  this: LLVMBranchUnreachable[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("unreachable")
}
