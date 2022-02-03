package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceJavaSupports, JavaTClass}

trait CoerceJavaSupportsImpl[G] { this: CoerceJavaSupports[G] => 
  override def target: JavaTClass[G] = JavaTClass(targetClass, Nil)
}
