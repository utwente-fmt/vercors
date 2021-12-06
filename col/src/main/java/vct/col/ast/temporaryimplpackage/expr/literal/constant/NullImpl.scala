package vct.col.ast.temporaryimplpackage.expr.literal.constant

import vct.col.ast.{Null, TNull, Type}

trait NullImpl { this: Null =>
  override def t: Type = TNull()
}