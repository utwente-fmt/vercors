package vct.col.ast.family.coercion

import vct.col.ast.{CoerceNullClass, TClass}

trait CoerceNullClassImpl[G] { this: CoerceNullClass[G] => 
  override def target: TClass[G] = TClass(targetClass)
}
