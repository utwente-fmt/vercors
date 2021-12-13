package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{Star, TResource, Type}

trait StarImpl[G] { this: Star[G] =>
  override def t: Type[G] = TResource()
}