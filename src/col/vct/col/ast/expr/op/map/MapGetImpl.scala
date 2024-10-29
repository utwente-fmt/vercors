package vct.col.ast.expr.op.map

import vct.col.ast.{MapGet, TMap, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence}
import vct.col.ast.ops.MapGetOps

trait MapGetImpl[G] extends MapGetOps[G] {
  this: MapGet[G] =>
  def mapType: TMap[G] = map.t.asMap.get
  override def t: Type[G] = mapType.value

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(map) <> "[" <> Doc.arg(k) <> "]")
}
