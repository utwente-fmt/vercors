package vct.col.ast.temporaryimplpackage.expr.op.either

import vct.col.ast.{EitherOp, Expr, TEither}

trait EitherOpImpl { this: EitherOp =>
  def either: Expr
  def eitherType: TEither = either.t.asEither.get
}