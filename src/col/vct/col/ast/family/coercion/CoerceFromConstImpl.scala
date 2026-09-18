package vct.col.ast.family.coercion

import vct.col.ast.{CoerceFromConst, TConst, Type}
import vct.col.ast.ops.CoerceFromConstOps
import vct.col.print._

trait CoerceFromConstImpl[G] extends CoerceFromConstOps[G] { this: CoerceFromConst[G] =>
  val source: Type[G] = TConst[G](target)
}
