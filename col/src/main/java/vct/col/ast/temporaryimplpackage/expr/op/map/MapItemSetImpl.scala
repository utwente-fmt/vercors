package vct.col.ast.temporaryimplpackage.expr.op.map

import vct.col.ast.{MapItemSet, TSet, TTuple, Type}

trait MapItemSetImpl[G] { this: MapItemSet[G] =>
  override def t: Type[G] = TSet(TTuple(Seq(mapType.key, mapType.value)))
}