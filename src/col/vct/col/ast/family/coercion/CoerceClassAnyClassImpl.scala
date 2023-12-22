package vct.col.ast.family.coercion

import vct.col.ast.{CoerceClassAnyClass, TAnyClass}
import vct.col.ast.ops.CoerceClassAnyClassOps

trait CoerceClassAnyClassImpl[G] extends CoerceClassAnyClassOps[G] { this: CoerceClassAnyClass[G] =>
  override def target: TAnyClass[G] = TAnyClass()
}
