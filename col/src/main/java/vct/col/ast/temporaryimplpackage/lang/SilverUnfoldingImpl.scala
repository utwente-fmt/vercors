package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{SilverUnfolding, Type}

trait SilverUnfoldingImpl[G] { this: SilverUnfolding[G] =>
  override def t: Type[G] = body.t
}