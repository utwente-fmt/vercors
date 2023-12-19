package vct.col.ast.expr.resource

import vct.col.ast.{ResourceOfResourceValue, TResource}

trait ResourceOfResourceValueImpl[G] { this: ResourceOfResourceValue[G] =>
  override def t: TResource[G] = TResource()
}
