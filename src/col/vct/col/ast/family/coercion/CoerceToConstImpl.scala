package vct.col.ast.family.coercion

import vct.col.ast.{CoerceToConst, TConst, Type}
import vct.col.ast.ops.CoerceToConstOps
import vct.col.print._

trait CoerceToConstImpl[G] extends CoerceToConstOps[G] { this: CoerceToConst[G] =>
  val target: Type[G] = TConst[G](source)
}
