package vct.col.ast.family.coercion

import vct.col.ast.{CoerceNullJavaClass, JavaTClass}
import vct.col.ast.ops.CoerceNullJavaClassOps

trait CoerceNullJavaClassImpl[G] extends CoerceNullJavaClassOps[G] {
  this: CoerceNullJavaClass[G] =>
  override def target: JavaTClass[G] = JavaTClass(targetClass, Nil)
}
