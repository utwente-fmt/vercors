package vct.col.ast.expr.binder

import vct.col.ast.{Starall, TResource, Type}

trait StarallImpl[G] { this: Starall[G] =>
  override def t: Type[G] = TResource()
}
