package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceMapBag, TBag}

trait CoerceMapBagImpl[G] { this: CoerceMapBag[G] => 
  override def target: TBag[G] = TBag(targetBagElement)
}
