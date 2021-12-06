package vct.col.ast.temporaryimplpackage.expr.binder

import vct.col.ast.{Starall, TResource, Type}

trait StarallImpl { this: Starall =>
  override def t: Type = TResource()
}
