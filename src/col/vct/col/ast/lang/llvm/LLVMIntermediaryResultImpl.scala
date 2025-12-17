package vct.col.ast.lang.llvm

import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.ops.LLVMIntermediaryResultOps
import vct.col.ast.{LLVMIntermediaryResult, Type}
import vct.col.print.Precedence

trait LLVMIntermediaryResultImpl[G]
    extends NodeFamilyImpl[G] with LLVMIntermediaryResultOps[G] {
  this: LLVMIntermediaryResult[G] =>

  override def t: Type[G] =
    sretArg match {
      case Some(rArg) => rArg.decl.t
      case None => applicable.decl.returnType
    }

  override def precedence: Int = Precedence.ATOMIC
}
