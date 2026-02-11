package vct.col.ast.family.coercion

import vct.col.ast.ops.CoerceToImmutableOps
import vct.col.ast.{CoerceToImmutable, TImmutable, Type}

trait CoerceToImmutableImpl[G] extends CoerceToImmutableOps[G] {
  this: CoerceToImmutable[G] =>
  val target: Type[G] = TImmutable[G](source)
}
