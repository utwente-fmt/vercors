package vct.col.ast.expr.op.map

import vct.col.ast.{MapCons, TMap, Type}

trait MapConsImpl[G] { this: MapCons[G] =>
  override def t: TMap[G] = tailType
  def tailType: TMap[G] = map.t.asMap.get
}