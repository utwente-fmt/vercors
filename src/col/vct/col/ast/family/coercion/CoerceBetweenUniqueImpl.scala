package vct.col.ast.family.coercion

import vct.col.ast.{CoerceBetweenUnique, TUnique}
import vct.col.ast.ops.CoerceBetweenUniqueOps
import vct.col.print._

trait CoerceBetweenUniqueImpl[G] extends CoerceBetweenUniqueOps[G] { this: CoerceBetweenUnique[G] =>
  override def target = TUnique(innerCoercion.target, targetId)
}
