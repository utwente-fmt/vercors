package vct.col.resolve.lang

import vct.col.ast.{ContractApplicable, Expr, JavaParam, Type, Variable}

case object Util {
  def compatJavaParams[G](args: Seq[Expr[G]], params: Seq[JavaParam[G]]): Boolean =
    compatTypes(args, params.map(_.t))

  def compat[G](args: Seq[Expr[G]], params: Seq[Variable[G]]): Boolean =
    compatTypes(args, params.map(_.t))

  def compatTypes[G](args: Seq[Expr[G]], types: Seq[Type[G]]): Boolean =
    args.size == types.size && types.zip(args).forall {
      case (t, e) => t.superTypeOf(e.t)
    }

  def compat[G](args: Seq[Expr[G]], typeArgs: Seq[Type[G]], genericInvokable: ContractApplicable[G]): Boolean =
    compat(args, typeArgs, genericInvokable.args, genericInvokable.typeArgs)

  def compat[G](args: Seq[Expr[G]], typeArgs: Seq[Type[G]], params: Seq[Variable[G]], typeParams: Seq[Variable[G]]): Boolean =
    args.size == params.size && typeArgs.size == typeParams.size &&
      params.zip(args).forall {
        case (v, e) => v.t.particularize(typeParams.zip(typeArgs).toMap).superTypeOf(e.t)
      }

  def compat[G](args: Seq[Expr[G]], params: Seq[Variable[G]], typeEnv: Map[Variable[G], Type[G]]): Boolean =
    args.size == params.size &&
      params.zip(args).forall {
        case (v, e) => v.t.particularize(typeEnv).superTypeOf(e.t)
      }
}
