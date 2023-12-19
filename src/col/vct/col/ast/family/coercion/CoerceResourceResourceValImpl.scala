package vct.col.ast.family.coercion

import vct.col.ast.{CoerceResourceResourceVal, TResourceVal}

trait CoerceResourceResourceValImpl[G] { this: CoerceResourceResourceVal[G] =>
  override def target: TResourceVal[G] = TResourceVal()
}
