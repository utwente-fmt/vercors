package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceSelectUnion, TUnion}

trait CoerceSelectUnionImpl[G] { this: CoerceSelectUnion[G] => 
  override def target: TUnion[G] = TUnion(targetAlts)
}
