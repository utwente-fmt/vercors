package vct.col.ast.family.coercion

import vct.col.ast.{CoerceSelectUnion, TUnion}

trait CoerceSelectUnionImpl[G] { this: CoerceSelectUnion[G] => 
  override def target: TUnion[G] = TUnion(targetAlts)
}
