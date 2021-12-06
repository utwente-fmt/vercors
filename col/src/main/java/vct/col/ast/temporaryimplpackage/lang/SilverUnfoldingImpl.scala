package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{SilverUnfolding, Type}

trait SilverUnfoldingImpl { this: SilverUnfolding =>
  override def t: Type = body.t
}