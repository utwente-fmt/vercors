package vct.col.ast.family.coercion

import vct.col.ast.{CoerceMapBag, TBag}
import vct.col.ast.ops.CoerceMapBagOps

trait CoerceMapBagImpl[G] extends CoerceMapBagOps[G] { this: CoerceMapBag[G] => 
  override def target: TBag[G] = TBag(targetBagElement)
}
