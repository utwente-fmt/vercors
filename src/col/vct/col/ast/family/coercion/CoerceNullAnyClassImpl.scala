package vct.col.ast.family.coercion

import vct.col.ast.{CoerceNullAnyClass, TAnyClass}
import vct.col.ast.ops.CoerceNullAnyClassOps

trait CoerceNullAnyClassImpl[G] extends CoerceNullAnyClassOps[G] { this: CoerceNullAnyClass[G] =>
  override def target: TAnyClass[G] = TAnyClass()
}
