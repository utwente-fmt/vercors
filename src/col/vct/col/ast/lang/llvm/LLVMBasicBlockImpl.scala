package vct.col.ast.lang.llvm

import vct.col.ast.LLVMBasicBlock
import vct.col.ast.ops.LLVMBasicBlockOps
import vct.col.check.{CheckContext, CheckError}

trait LLVMBasicBlockImpl[G] extends LLVMBasicBlockOps[G] {
  this: LLVMBasicBlock[G] =>

  override def check(context: CheckContext[G]): Seq[CheckError] = Nil
}
