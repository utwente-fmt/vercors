package vct.col.ast.lang.llvm

import vct.col.ast.{LLVMTPointer, LLVMWrapperInvocation, Type}
import vct.col.ast.ops.{LLVMFunctionInvocationOps, LLVMWrapperInvocationOps}
import vct.col.print._

trait LLVMWrapperInvocationImpl[G] extends LLVMWrapperInvocationOps[G] {
  this: LLVMWrapperInvocation[G] =>
  override def precedence: Int = Precedence.POSTFIX

  override def t: Type[G] = {
    ref.decl.sretArg match {
      case Some(retArg) => LLVMTPointer(retArg.sretType)
      case None => ref.decl.returnType
    }
  }

  override def layout(implicit ctx: Ctx): Doc =
    Group(Group(Text("@") <> ctx.name(ref) <> "(") <> Doc.args(callArgs) <> ")")
}
