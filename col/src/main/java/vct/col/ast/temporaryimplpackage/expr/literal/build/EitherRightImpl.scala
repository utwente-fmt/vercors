package vct.col.ast.temporaryimplpackage.expr.literal.build

import vct.col.ast.{EitherRight, TEither, TNothing, Type}

trait EitherRightImpl[G] { this: EitherRight[G] =>
  override def t: Type[G] = TEither(TNothing(), e.t)
}