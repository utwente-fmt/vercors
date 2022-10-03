package vct.col.ast.family.coercion

import vct.col.ast.{CoerceMapBag, TBag}

trait CoerceMapBagImpl[G] { this: CoerceMapBag[G] => 
  override def target: TBag[G] = TBag(targetBagElement)
}
