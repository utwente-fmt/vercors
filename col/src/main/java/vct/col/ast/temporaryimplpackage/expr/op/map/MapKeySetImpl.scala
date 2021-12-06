package vct.col.ast.temporaryimplpackage.expr.op.map

import vct.col.ast.{MapKeySet, TSet, Type}

trait MapKeySetImpl { this: MapKeySet =>
  override def t: Type = TSet(mapType.key)
}