package vct.col.ast.expr.op.map

import vct.col.ast.{MapGet, TMap, Type}

trait MapGetImpl[G] { this: MapGet[G] =>
  def mapType: TMap[G] = map.t.asMap.get
  override def t: Type[G] = mapType.value
}