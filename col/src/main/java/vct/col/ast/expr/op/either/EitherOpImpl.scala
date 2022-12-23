package vct.col.ast.expr.op.either

import vct.col.ast.{EitherOp, Expr, TEither}

trait EitherOpImpl[G] { this: EitherOp[G] =>
  def either: Expr[G]
  def eitherType: TEither[G] = either.t.asEither.get
}