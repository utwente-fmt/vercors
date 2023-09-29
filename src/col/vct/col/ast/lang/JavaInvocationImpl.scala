package vct.col.ast.lang

import vct.col.ast.{JavaInvocation, Type}
import vct.col.print.{Ctx, Doc, DocUtil, Empty, Group, Precedence, Show, Text}
import vct.col.resolve.ctx._
import vct.col.util.AstBuildHelpers.tt

trait JavaInvocationImpl[G] { this: JavaInvocation[G] =>
  override lazy val t: Type[G] = ref.get match {
    case RefFunction(decl) => decl.returnType
    case RefProcedure(decl) => decl.returnType
    case RefPredicate(decl) => decl.returnType
    case RefInstanceFunction(decl) => decl.returnType
    case RefInstanceMethod(decl) => decl.returnType
    case RefInstancePredicate(decl) => decl.returnType
    case RefADTFunction(decl) => decl.returnType
    case RefModelProcess(decl) => decl.returnType
    case RefModelAction(decl) => decl.returnType
    case RefJavaMethod(decl) => decl.returnType
    case RefProverFunction(decl) => decl.returnType
    case BuiltinInstanceMethod(f) => f(obj.get)(arguments).t
    case RefJavaAnnotationMethod(decl) => decl.returnType
    case RefLlvmSpecFunction(decl) => decl.returnType
  }

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    obj.map(obj => assoc(obj) <> ".").getOrElse(Empty) <>
      method <>
      (if(typeParams.isEmpty) Empty else Text("<") <> Doc.args(typeParams) <> ">") <>
      "(" <> Doc.args(arguments) <> ")" <> DocUtil.givenYields(givenArgs, yields)
}