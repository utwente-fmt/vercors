package vct.col.ast.lang.llvm

import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.ops.LLVMResultOps
import vct.col.ast.{LLVMResult, LLVMTPointer, Type}
import vct.col.print.Precedence

trait LLVMResultImpl[G] extends NodeFamilyImpl[G] with LLVMResultOps[G] {
  this: LLVMResult[G] =>

  override def t: Type[G] = {
    func.decl.sretArg match {
      case Some(retArg) => LLVMTPointer(retArg.sretType)
      case None => func.decl.returnType
    }
  }

  override def precedence: Int = Precedence.ATOMIC
}
