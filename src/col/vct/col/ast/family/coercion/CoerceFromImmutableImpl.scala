package vct.col.ast.family.coercion

import vct.col.ast.ops.CoerceFromImmutableOps
import vct.col.ast.{CoerceFromImmutable, TImmutable, Type}

trait CoerceFromImmutableImpl[G] extends CoerceFromImmutableOps[G] {
  this: CoerceFromImmutable[G] =>
  val source: Type[G] = TImmutable[G](target)
}
