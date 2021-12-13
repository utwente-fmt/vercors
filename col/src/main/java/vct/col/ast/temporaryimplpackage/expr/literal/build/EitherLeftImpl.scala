package vct.col.ast.temporaryimplpackage.expr.literal.build

import vct.col.ast.{EitherLeft, TEither, TNothing, Type}

trait EitherLeftImpl[G] { this: EitherLeft[G] =>
  override def t: Type[G] = TEither(e.t, TNothing())
}