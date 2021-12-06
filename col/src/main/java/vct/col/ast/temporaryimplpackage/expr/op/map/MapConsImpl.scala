package vct.col.ast.temporaryimplpackage.expr.op.map

import vct.col.ast.{MapCons, TMap, Type}

trait MapConsImpl { this: MapCons =>
  override def t: Type = tailType
  def tailType: TMap = map.t.asMap.get
}