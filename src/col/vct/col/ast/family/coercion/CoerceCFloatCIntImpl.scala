package vct.col.ast.family.coercion

import vct.col.ast.{CoerceCFloatCInt, TCInt}

trait CoerceCFloatCIntImpl[G] { this: CoerceCFloatCInt[G] =>
  override def target: TCInt[G] = TCInt()
}
