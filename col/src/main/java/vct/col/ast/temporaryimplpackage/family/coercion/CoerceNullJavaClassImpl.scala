package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceNullJavaClass, JavaTClass}

trait CoerceNullJavaClassImpl[G] { this: CoerceNullJavaClass[G] => 
  override def target: JavaTClass[G] = JavaTClass(targetClass, Nil)
}
