package vct.col.ast.temporaryimplpackage.expr.literal.constant

import vct.col.ast.{Null, TNull, Type}

trait NullImpl[G] { this: Null[G] =>
  override def t: Type[G] = TNull()
}