package vct.col.ast.family.coercion

import vct.col.ast.{CoerceTArrayAnyClass, TAnyClass}

trait CoerceTArrayAnyClassImpl[G] { this: CoerceTArrayAnyClass[G] =>
  override def target: TAnyClass[G] = TAnyClass()
}
