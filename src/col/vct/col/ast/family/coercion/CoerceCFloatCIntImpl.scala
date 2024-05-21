package vct.col.ast.family.coercion

import vct.col.ast.{CoerceCFloatCInt, TCInt}
import vct.col.ast.ops.CoerceCFloatCIntOps

trait CoerceCFloatCIntImpl[G] extends CoerceCFloatCIntOps[G] { this: CoerceCFloatCInt[G] =>
  override def target: TCInt[G] = TCInt()
}
