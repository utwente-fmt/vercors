package vct.col.ast.family.coercion

import vct.col.ast.{CoerceMapMap, TMap}

trait CoerceMapMapImpl[G] { this: CoerceMapMap[G] => 
  def target: TMap[G] = TMap(targetTypes._1, targetTypes._2)
}
