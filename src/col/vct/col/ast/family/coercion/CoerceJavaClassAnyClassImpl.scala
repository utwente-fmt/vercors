package vct.col.ast.family.coercion

import vct.col.ast.{CoerceJavaClassAnyClass, TAnyClass}
import vct.col.ast.ops.CoerceJavaClassAnyClassOps

trait CoerceJavaClassAnyClassImpl[G] extends CoerceJavaClassAnyClassOps[G] { this: CoerceJavaClassAnyClass[G] =>
  override def target: TAnyClass[G] = TAnyClass()
}
