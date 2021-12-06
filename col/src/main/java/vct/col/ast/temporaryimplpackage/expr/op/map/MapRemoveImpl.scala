package vct.col.ast.temporaryimplpackage.expr.op.map

import vct.col.ast.{MapRemove, TMap, Type}

trait MapRemoveImpl { this: MapRemove =>
  def mapType: TMap = map.t.asMap.get
  override def t: Type = mapType
}