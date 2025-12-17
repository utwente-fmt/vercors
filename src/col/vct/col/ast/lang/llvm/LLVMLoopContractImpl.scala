package vct.col.ast.lang.llvm

import vct.col.ast.LLVMLoopContract
import vct.col.ast.ops.LLVMLoopContractOps
import vct.col.print._

trait LLVMLoopContractImpl[G] extends LLVMLoopContractOps[G] {
  this: LLVMLoopContract[G] =>

  override def layout(implicit ctx: Ctx): Doc = {
    Doc.spec(DocUtil.clauses("loop_invariant", invariant))
  }
}
