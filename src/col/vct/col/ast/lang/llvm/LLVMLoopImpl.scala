package vct.col.ast.lang.llvm

import vct.col.ast.LLVMLoop
import vct.col.ast.ops.LLVMLoopOps
import vct.col.check.{CheckContext, CheckError}
import vct.col.ast.ops.{LLVMLoopOps, LLVMLoopFamilyOps}

trait LLVMLoopImpl[G] extends LLVMLoopOps[G] with LLVMLoopFamilyOps[G] {
  this: LLVMLoop[G] =>

  override def check(context: CheckContext[G]): Seq[CheckError] = Nil
}
