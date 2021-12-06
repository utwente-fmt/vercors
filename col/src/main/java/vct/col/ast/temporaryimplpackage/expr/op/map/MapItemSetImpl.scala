package vct.col.ast.temporaryimplpackage.expr.op.map

import vct.col.ast.{MapItemSet, TSet, TTuple, Type}

trait MapItemSetImpl { this: MapItemSet =>
  override def t: Type = TSet(TTuple(Seq(mapType.key, mapType.value)))
}