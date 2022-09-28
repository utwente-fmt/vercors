package vct.col.resolve.lang

import vct.col.ast.{ContractApplicable, Expr, Type, Variable}

case object Util {
  def compat[G](args: Seq[Expr[G]], params: Seq[Variable[G]]): Boolean =
    args.size == params.size && params.zip(args).forall {
      case (v, e) => v.t.superTypeOf(e.t)
    }

  def compat[G](args: Seq[Expr[G]], typeArgs: Seq[Type[G]], genericInvokable: ContractApplicable[G]): Boolean =
    args.size == genericInvokable.args.size && typeArgs.size == genericInvokable.typeArgs.size &&
      genericInvokable.args.zip(args).forall {
        case (v, e) => v.t.particularize(genericInvokable.typeArgs.zip(typeArgs).toMap).superTypeOf(e.t)
      }
}
