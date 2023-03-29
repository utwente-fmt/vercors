package vct.col.ast.expr.op.map

import vct.col.ast.{MapRemove, TMap, Type}

trait MapRemoveImpl[G] { this: MapRemove[G] =>
  def mapType: TMap[G] = map.t.asMap.get
  override def t: Type[G] = mapType
}