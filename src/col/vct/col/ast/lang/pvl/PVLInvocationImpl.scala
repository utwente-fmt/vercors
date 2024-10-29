package vct.col.ast.lang.pvl

import vct.col.ast.{
  Applicable,
  ContractApplicable,
  PVLInvocation,
  TClass,
  TProcess,
  TResource,
  Type,
  Variable,
}
import vct.col.print.{Ctx, Doc, DocUtil, Empty, Group, Text}
import vct.col.resolve.ctx._
import vct.col.ast.ops.PVLInvocationOps
import vct.col.ref.Ref

trait PVLInvocationImpl[G] extends PVLInvocationOps[G] {
  this: PVLInvocation[G] =>
  override lazy val t: Type[G] =
    ref.get match {
      case RefFunction(decl) => returnType(decl)
      case RefProcedure(decl) => returnType(decl)
      case RefPredicate(_) => TResource()
      case RefInstanceFunction(decl) => returnType(decl)
      case RefInstanceMethod(decl) => returnType(decl)
      case RefInstancePredicate(_) => TResource()
      case RefADTFunction(decl) => decl.returnType
      case RefModelProcess(_) => TProcess()
      case RefModelAction(_) => TProcess()
      case RefProverFunction(decl) => decl.returnType
      case PVLBuiltinInstanceMethod(f) => f(obj.get)(args).t
      case BuiltinInstanceMethod(f) => f(obj.get)(args).t
    }

  private def returnType(app: ContractApplicable[G]): Type[G] =
    app.returnType.particularize(typeEnv(app))

  // Take care to include type arguments of both the type of the object the method is called on, as well as
  // the arguments given to the method call itself.
  private def typeEnv(app: ContractApplicable[G]): Map[Variable[G], Type[G]] =
    app.typeArgs.zip(typeArgs).toMap ++ obj.flatMap(_.t.asClass).map(_.typeEnv)
      .getOrElse(Map.empty)

  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Group(
        Group(
          obj.map(assoc(_) <> ".").getOrElse(Empty) <> method <>
            (if (typeArgs.isEmpty)
               Empty
             else
               Text("<") <> Doc.args(typeArgs) <> ">")
        ) <> "(" <> Doc.args(args) <> ")"
      ) <> DocUtil.givenYields(givenMap, yields)
    )
}
