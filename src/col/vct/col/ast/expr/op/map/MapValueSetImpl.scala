package vct.col.ast.expr.op.map

import vct.col.ast.{MapValueSet, TSet, Type}

trait MapValueSetImpl[G] { this: MapValueSet[G] =>
  override def t: Type[G] = TSet(mapType.value)
}