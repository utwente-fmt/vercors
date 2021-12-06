package vct.col.ast.temporaryimplpackage.expr.op.map

import vct.col.ast.{MapGet, TMap, Type}

trait MapGetImpl { this: MapGet =>
  def mapType: TMap = map.t.asMap.get
  override def t: Type = mapType.value
}