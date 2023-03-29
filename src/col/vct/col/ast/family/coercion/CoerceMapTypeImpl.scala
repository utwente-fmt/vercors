package vct.col.ast.family.coercion

import vct.col.ast.{CoerceMapType, TType}

trait CoerceMapTypeImpl[G] { this: CoerceMapType[G] => 
  def target: TType[G] = TType(targetBound)
}
