package vct.col.ast.family.coercion

import vct.col.ast.{CoerceResourceValResource, TResource}

trait CoerceResourceValResourceImpl[G] { this: CoerceResourceValResource[G] =>
  override def target: TResource[G] = TResource()
}
