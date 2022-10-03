package vct.col.ast.expr.literal.build

import vct.col.ast.{LiteralMap, TMap, Type}

trait LiteralMapImpl[G] { this: LiteralMap[G] =>
  override def t: Type[G] = TMap(k, v)
}