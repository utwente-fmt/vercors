package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{Star, TResource, Type}

trait StarImpl { this: Star =>
  override def t: Type = TResource()
}