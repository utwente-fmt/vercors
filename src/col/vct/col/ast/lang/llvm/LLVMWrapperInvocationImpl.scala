package vct.col.ast.lang.llvm

import vct.col.ast.{LLVMWrapperInvocation, Type}
import vct.col.ast.ops.{LLVMFunctionInvocationOps, LLVMWrapperInvocationOps}
import vct.col.print._

trait LLVMWrapperInvocationImpl[G] extends LLVMWrapperInvocationOps[G] {
  this: LLVMWrapperInvocation[G] =>
  override def precedence: Int = Precedence.POSTFIX

  override def t: Type[G] =
    if (ref.decl.returnInParam.nonEmpty) { ref.decl.returnInParam.get._2 }
    else { ref.decl.returnType }

  override def layout(implicit ctx: Ctx): Doc =
    Group(Group(Text("@") <> ctx.name(ref) <> "(") <> Doc.args(callArgs) <> ")")
}
