package vct.col.ast.family.coercion

import vct.col.ast.{CoerceToUnique, TUnique}
import vct.col.ast.ops.CoerceToUniqueOps
import vct.col.print._

trait CoerceToUniqueImpl[G] extends CoerceToUniqueOps[G] { this: CoerceToUnique[G] =>
  override def target = TUnique(source, targetId)
}
