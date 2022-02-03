package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceSupports, TClass}

trait CoerceSupportsImpl[G] { this: CoerceSupports[G] => 
  override def target: TClass[G] = TClass(targetClass)
}
