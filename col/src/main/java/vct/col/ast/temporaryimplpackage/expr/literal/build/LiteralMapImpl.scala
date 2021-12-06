package vct.col.ast.temporaryimplpackage.expr.literal.build

import vct.col.ast.{LiteralMap, TMap, Type}

trait LiteralMapImpl { this: LiteralMap =>
  override def t: Type = TMap(k, v)
}