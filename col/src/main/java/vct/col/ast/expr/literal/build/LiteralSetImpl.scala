package vct.col.ast.expr.literal.build

import vct.col.ast.{LiteralSet, TSet, Type}

trait LiteralSetImpl[G] { this: LiteralSet[G] =>
  override def t: Type[G] = TSet(element)
}