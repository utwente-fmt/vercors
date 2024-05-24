package vct.col.ast.family.coercion

import vct.col.ast.{CoerceResourceValResource, TResource}
import vct.col.ast.ops.CoerceResourceValResourceOps

trait CoerceResourceValResourceImpl[G] extends CoerceResourceValResourceOps[G] { this: CoerceResourceValResource[G] =>
  override def target: TResource[G] = TResource()
}
