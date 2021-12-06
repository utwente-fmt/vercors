package vct.col.ast.temporaryimplpackage.expr.op.map

import vct.col.ast.{Expr, MapOp, TMap}

trait MapOpImpl { this: MapOp =>
  def map: Expr
  def mapType: TMap = map.t.asMap.get
}