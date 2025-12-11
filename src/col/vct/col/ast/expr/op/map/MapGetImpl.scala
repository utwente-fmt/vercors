package vct.col.ast.expr.op.map

import vct.col.ast.{MapGet, TMap, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.MapGetOps

trait MapGetImpl[G] extends MapGetOps[G] {
  this: MapGet[G] =>
  def mapType: TMap[G] = map.t.asMap.get
  override def t: Type[G] = mapType.value

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = {
    ctx.syntax match {
      case Ctx.Isar => Group(Text("lookup") <+> assoc(map) <+> Doc.arg(k))
      case _ => Group(assoc(map) <> "[" <> Doc.arg(k) <> "]")
    }
  }
}
