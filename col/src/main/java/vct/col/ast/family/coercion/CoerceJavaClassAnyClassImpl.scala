package vct.col.ast.family.coercion

import vct.col.ast.{CoerceJavaClassAnyClass, TAnyClass}

trait CoerceJavaClassAnyClassImpl[G] { this: CoerceJavaClassAnyClass[G] =>
  override def target: TAnyClass[G] = TAnyClass()
}
