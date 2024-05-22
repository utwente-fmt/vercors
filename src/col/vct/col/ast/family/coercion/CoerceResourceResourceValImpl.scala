package vct.col.ast.family.coercion

import vct.col.ast.{CoerceResourceResourceVal, TResourceVal}
import vct.col.ast.ops.CoerceResourceResourceValOps

trait CoerceResourceResourceValImpl[G] extends CoerceResourceResourceValOps[G] { this: CoerceResourceResourceVal[G] =>
  override def target: TResourceVal[G] = TResourceVal()
}
