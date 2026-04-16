package vct.col.ast.expr.op.map

import vct.col.ast.{MapItemSet, TSet, TTuple, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.MapItemSetOps

trait MapItemSetImpl[G] extends MapItemSetOps[G] {
  this: MapItemSet[G] =>
  override def t: Type[G] = TSet(TTuple(Seq(mapType.key, mapType.value)))

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = {
    ctx.syntax match {
      case Ctx.Isar => Text("fset_of_fmap") <+> assoc(map)
      case _ => assoc(map) <> ".items"
    }
  }
}
