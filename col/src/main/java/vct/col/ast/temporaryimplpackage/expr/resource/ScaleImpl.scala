package vct.col.ast.temporaryimplpackage.expr.resource

import vct.col.ast.{Scale, TResource, Type}

trait ScaleImpl { this: Scale =>
  override def t: Type = TResource()
}