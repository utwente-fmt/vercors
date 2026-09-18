package vct.col.ast.family.coercion

import vct.col.ast.{CoerceNullClass, TClass}
import vct.col.ast.ops.CoerceNullClassOps

trait CoerceNullClassImpl[G] extends CoerceNullClassOps[G] {
  this: CoerceNullClass[G] =>
  override def target: TClass[G] = targetClass.decl.classType(typeArgs)
}
