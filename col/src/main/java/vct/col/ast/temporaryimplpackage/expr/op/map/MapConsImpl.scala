package vct.col.ast.temporaryimplpackage.expr.op.map

import vct.col.ast.{MapCons, TMap, Type}

trait MapConsImpl[G] { this: MapCons[G] =>
  override def t: Type[G] = tailType
  def tailType: TMap[G] = map.t.asMap.get
}