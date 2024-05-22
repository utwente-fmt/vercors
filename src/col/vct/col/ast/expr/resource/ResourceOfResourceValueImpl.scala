package vct.col.ast.expr.resource

import vct.col.ast.{ResourceOfResourceValue, TResource}
import vct.col.ast.ops.ResourceOfResourceValueOps

trait ResourceOfResourceValueImpl[G] extends ResourceOfResourceValueOps[G] { this: ResourceOfResourceValue[G] =>
  override def t: TResource[G] = TResource()
}
