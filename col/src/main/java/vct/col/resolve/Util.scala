package vct.col.resolve

import vct.col.ast.{Expr, Variable}

case object Util {
  def compat(args: Seq[Expr], params: Seq[Variable]): Boolean =
    args.size == params.size && params.zip(args).forall {
      case (v, e) => v.t.superTypeOf(e.t)
    }
}
