package vct.col.ast.temporaryimplpackage.expr.literal.build

import vct.col.ast.{LiteralSet, TSet, Type}

trait LiteralSetImpl { this: LiteralSet =>
  override def t: Type = TSet(element)
}