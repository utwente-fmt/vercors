package vct.col.ast.temporaryimplpackage.expr.op.map

import vct.col.ast.{MapValueSet, TSet, Type}

trait MapValueSetImpl { this: MapValueSet =>
  override def t: Type = TSet(mapType.value)
}