package vct.col.resolve

import vct.col.ast.Type
import vct.col.ast.{ContractApplicable, Expr, Variable}

case object Util {
  def compat(args: Seq[Expr], params: Seq[Variable]): Boolean =
    args.size == params.size && params.zip(args).forall {
      case (v, e) => v.t.superTypeOf(e.t)
    }

  def compat(args: Seq[Expr], typeArgs: Seq[Type], genericInvokable: ContractApplicable): Boolean =
    args.size == genericInvokable.args.size && typeArgs.size == genericInvokable.typeArgs.size &&
      genericInvokable.args.zip(args).forall {
        case (v, e) => v.t.particularize(genericInvokable.typeArgs.zip(typeArgs).toMap).superTypeOf(e.t)
      }
}
