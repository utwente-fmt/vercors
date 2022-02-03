package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceMapType, TType}

trait CoerceMapTypeImpl[G] { this: CoerceMapType[G] => 
  def target: TType[G] = TType(targetBound)
}
