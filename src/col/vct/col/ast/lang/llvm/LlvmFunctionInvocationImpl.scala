package vct.col.ast.lang.llvm

import vct.col.ast.LlvmFunctionInvocation
import vct.col.print.{Ctx, Doc, DocUtil, Empty, Group, Precedence, Text}
import vct.col.ast.ops.LlvmFunctionInvocationOps

trait LlvmFunctionInvocationImpl[G] extends LlvmFunctionInvocationOps[G] {
  this: LlvmFunctionInvocation[G] =>
  override def precedence: Int = Precedence.POSTFIX

  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Group(Text(ctx.name(ref)) <> "(") <> Doc.args(args) <> ")" <>
        DocUtil.givenYields(givenMap, yields)
    )
}
