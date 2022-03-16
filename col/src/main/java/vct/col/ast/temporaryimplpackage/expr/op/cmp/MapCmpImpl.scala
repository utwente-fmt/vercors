package vct.col.ast.temporaryimplpackage.expr.op.cmp

import vct.col.ast.{Expr, MapCmp, TMap}
import vct.col.util.Types

trait MapCmpImpl[G] { this: MapCmp[G] =>
  def left: Expr[G]
  def right: Expr[G]

  def leftT: TMap[G] = left.t.asMap.get
  def rightT: TMap[G] = right.t.asMap.get

  def commonMapType: TMap[G] = TMap(leftT.key, Types.leastCommonSuperType(leftT.value, rightT.value))
}
