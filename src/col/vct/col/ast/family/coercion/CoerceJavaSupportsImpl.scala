package vct.col.ast.family.coercion

import vct.col.ast.{CoerceJavaSupports, JavaTClass}
import vct.col.ast.ops.CoerceJavaSupportsOps

trait CoerceJavaSupportsImpl[G] extends CoerceJavaSupportsOps[G] {
  this: CoerceJavaSupports[G] =>
  override def target: JavaTClass[G] = JavaTClass(targetClass, Nil)
}
