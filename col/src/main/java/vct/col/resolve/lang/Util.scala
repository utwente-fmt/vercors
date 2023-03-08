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
    args.size == genericInvokable.args.size && typeArgs.size == genericInvokable.typeArgs.size &&
      genericInvokable.args.zip(args).forall {
        case (v, e) => v.t.particularize(genericInvokable.typeArgs.zip(typeArgs).toMap).superTypeOf(e.t)
      }
}
