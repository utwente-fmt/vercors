package vct.col.ast.temporaryimplpackage.expr.op.map

import vct.col.ast.{Expr, MapOp, TMap}

trait MapOpImpl[G] { this: MapOp[G] =>
  def map: Expr[G]
  def mapType: TMap[G] = map.t.asMap.get
}