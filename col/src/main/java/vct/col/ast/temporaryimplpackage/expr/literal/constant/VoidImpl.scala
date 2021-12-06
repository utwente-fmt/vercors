package vct.col.ast.temporaryimplpackage.expr.literal.constant

import vct.col.ast.{TVoid, Type, Void}

trait VoidImpl { this: Void =>
  override def t: Type = TVoid()
}