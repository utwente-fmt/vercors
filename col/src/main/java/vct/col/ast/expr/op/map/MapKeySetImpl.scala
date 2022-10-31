package vct.col.ast.expr.op.map

import vct.col.ast.{MapKeySet, TSet, Type}

trait MapKeySetImpl[G] { this: MapKeySet[G] =>
  override def t: Type[G] = TSet(mapType.key)
}