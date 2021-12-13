package vct.col.ast.temporaryimplpackage.expr.literal.build

import vct.col.ast.{LiteralBag, TBag, Type}

trait LiteralBagImpl[G] { this: LiteralBag[G] =>
  override def t: Type[G] = TBag(element)
}