package vct.col.ast.temporaryimplpackage.expr.literal.build

import vct.col.ast.{EitherRight, TEither, TNothing, Type}

trait EitherRightImpl { this: EitherRight =>
  override def t: Type = TEither(TNothing(), e.t)
}