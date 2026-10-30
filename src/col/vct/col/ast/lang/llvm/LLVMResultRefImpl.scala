package vct.col.ast.lang.llvm

import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.ops.LLVMResultRefOps
import vct.col.ast.{LLVMResultRef, LLVMTPointer, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait LLVMResultRefImpl[G] extends NodeFamilyImpl[G] with LLVMResultRefOps[G] {
  this: LLVMResultRef[G] =>

  override def t: Type[G] = {
    func.decl.sretArg match {
      case Some(retArg) => LLVMTPointer(retArg.sretType)
      case None => LLVMTPointer(Some(func.decl.returnType))
    }
  }

  override def precedence: Int = Precedence.ATOMIC

  override def layout(implicit ctx: Ctx): Doc = Text("\\resultRef")

}
