package vct.col.ast.temporaryimplpackage.expr.literal.constant

import vct.col.ast.{TVoid, Type, Void}

trait VoidImpl[G] { this: Void[G] =>
  override def t: Type[G] = TVoid()
}