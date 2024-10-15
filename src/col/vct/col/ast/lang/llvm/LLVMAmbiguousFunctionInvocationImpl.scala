package vct.col.ast.lang.llvm

import vct.col.ast.{
  LLVMAmbiguousFunctionInvocation,
  LLVMFunctionDefinition,
  LLVMSpecFunction,
  Type,
}
import vct.col.print.{Ctx, Doc, DocUtil, Group, Precedence, Text}
import vct.col.ast.ops.LLVMAmbiguousFunctionInvocationOps

trait LLVMAmbiguousFunctionInvocationImpl[G]
    extends LLVMAmbiguousFunctionInvocationOps[G] {
  this: LLVMAmbiguousFunctionInvocation[G] =>
  override lazy val t: Type[G] =
    ref.get.decl match {
      case func: LLVMFunctionDefinition[G] => func.returnType
      case func: LLVMSpecFunction[G] => func.returnType
    }

  override def precedence: Int = Precedence.POSTFIX

  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Group(Text(name) <> "(") <> Doc.args(args) <> ")" <>
        DocUtil.givenYields(givenMap, yields)
    )
}
