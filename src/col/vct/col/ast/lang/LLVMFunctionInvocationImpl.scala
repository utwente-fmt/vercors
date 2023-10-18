package vct.col.ast.lang

import vct.col.ast.LlvmFunctionInvocation
import vct.col.print.{Ctx, Doc, DocUtil, Empty, Group, Precedence, Text}

trait LLVMFunctionInvocationImpl[G] {
  this: LlvmFunctionInvocation[G] =>
  override def precedence: Int = Precedence.POSTFIX

  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Group(Text(ctx.name(ref)) <> "(") <> Doc.args(args) <> ")" <>
        DocUtil.givenYields(givenMap, yields)
    )
}
