package vct.col.ast.lang.llvm

import vct.col.ast.{LLVMGhostAssign, LLVMWrapperInvocation}
import vct.col.ast.ops.LLVMGhostAssignOps
import vct.col.ast.statement.terminal.AssignStmtImpl
import vct.col.check.{CheckContext, CheckError, LLVMInvalidGhostAssign}

trait LLVMGhostAssignImpl[G]
    extends AssignStmtImpl[G] with LLVMGhostAssignOps[G] {
  this: LLVMGhostAssign[G] =>

  override def check(context: CheckContext[G]): Seq[CheckError] = {

    val wrongValueType =
      value match {
        case inv: LLVMWrapperInvocation[G] => Nil // Ok
        case _ => Seq(LLVMInvalidGhostAssign(this))
      }

    super.check(context) ++ wrongValueType
  }
}
