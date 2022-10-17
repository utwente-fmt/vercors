package vct.col.ast.family.coercion

import vct.col.ast.{CoerceClassAnyClass, TAnyClass}

trait CoerceClassAnyClassImpl[G] { this: CoerceClassAnyClass[G] =>
  override def target: TAnyClass[G] = TAnyClass()
}
