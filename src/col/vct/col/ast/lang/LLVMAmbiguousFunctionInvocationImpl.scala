package vct.col.ast.lang

import vct.col.ast.{LlvmAmbiguousFunctionInvocation, LlvmFunctionDefinition, LlvmSpecFunction, Type}
import vct.col.print.{Ctx, Doc, DocUtil, Group, Precedence, Text}

trait LLVMAmbiguousFunctionInvocationImpl[G] { this: LlvmAmbiguousFunctionInvocation[G] =>
  override lazy val t: Type[G] = ref.get.decl match {
    case func: LlvmFunctionDefinition[G] => func.returnType
    case func: LlvmSpecFunction[G] => func.returnType
  }

  override def precedence: Int = Precedence.POSTFIX

  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Group(
        Text(name) <> "(") <> Doc.args(args) <> ")" <> DocUtil.givenYields(givenMap, yields)
    )
}
