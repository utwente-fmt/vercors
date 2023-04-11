package vct.col.ast.lang

import vct.col.ast.{PVLInvocation, TProcess, TResource, Type}
import vct.col.print.{Ctx, Doc, DocUtil, Empty, Text, Group}
import vct.col.resolve.ctx._

trait PVLInvocationImpl[G] { this: PVLInvocation[G] =>
  override lazy val t: Type[G] = ref.get match {
    case RefFunction(decl) => decl.returnType.particularize(decl.typeArgs.zip(typeArgs).toMap)
    case RefProcedure(decl) => decl.returnType
    case RefPredicate(_) => TResource()
    case RefInstanceFunction(decl) => decl.returnType.particularize(decl.typeArgs.zip(typeArgs).toMap)
    case RefInstanceMethod(decl) => decl.returnType
    case RefInstancePredicate(_) => TResource()
    case RefADTFunction(decl) => decl.returnType
    case RefModelProcess(_) => TProcess()
    case RefModelAction(_) => TProcess()
    case PVLBuiltinInstanceMethod(f) => f(obj.get)(args).t
    case BuiltinInstanceMethod(f) => f(obj.get)(args).t
  }

  override def layout(implicit ctx: Ctx): Doc =
    Group(Group(Group(obj.map(assoc(_) <> ".").getOrElse(Empty) <>
      method <>
      (if(typeArgs.isEmpty) Empty else Text("<") <> Doc.args(typeArgs) <> ">")) <>
      "(" <> Doc.args(args) <> ")") <>
      DocUtil.givenYields(givenMap, yields))
}