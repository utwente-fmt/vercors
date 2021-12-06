package vct.col.ast.temporaryimplpackage.expr.literal.build

import vct.col.ast.{LiteralBag, TBag, Type}

trait LiteralBagImpl { this: LiteralBag =>
  override def t: Type = TBag(element)
}