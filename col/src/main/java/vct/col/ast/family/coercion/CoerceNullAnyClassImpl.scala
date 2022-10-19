package vct.col.ast.family.coercion

import vct.col.ast.{CoerceNullAnyClass, TAnyClass}

trait CoerceNullAnyClassImpl[G] { this: CoerceNullAnyClass[G] =>
  override def target: TAnyClass[G] = TAnyClass()
}
