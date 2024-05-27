package vct.col.ast.expr.op.map

import vct.col.ast.{MapRemove, TMap, Type}
import vct.col.print.{Ctx, Doc, Precedence, Group}
import vct.col.ast.ops.MapRemoveOps

trait MapRemoveImpl[G] extends MapRemoveOps[G] {
  this: MapRemove[G] =>
  def mapType: TMap[G] = map.t.asMap.get
  override def t: Type[G] = mapType

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(map) <> ".remove(" <> Doc.arg(k) <> ")")
}
