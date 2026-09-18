package vct.col.ast.lang.llvm

import vct.col.ast.LLVMFunctionInvocation
import vct.col.print.{Ctx, Doc, DocUtil, Empty, Group, Precedence, Text}
import vct.col.ast.ops.LLVMFunctionInvocationOps

trait LLVMFunctionInvocationImpl[G] extends LLVMFunctionInvocationOps[G] {
  this: LLVMFunctionInvocation[G] =>
  override def precedence: Int = Precedence.POSTFIX

  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Group(Text("@") <> ctx.name(ref) <> "(") <> Doc.args(args) <> ")" <>
        DocUtil.givenYields(givenMap, yields)
    )
}
