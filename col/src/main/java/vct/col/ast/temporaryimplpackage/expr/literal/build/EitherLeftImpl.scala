package vct.col.ast.temporaryimplpackage.expr.literal.build

import vct.col.ast.{EitherLeft, TEither, TNothing, Type}

trait EitherLeftImpl { this: EitherLeft =>
  override def t: Type = TEither(e.t, TNothing())
}