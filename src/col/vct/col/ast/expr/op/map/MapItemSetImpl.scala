package vct.col.ast.expr.op.map

import vct.col.ast.{MapItemSet, TSet, TTuple, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait MapItemSetImpl[G] { this: MapItemSet[G] =>
  override def t: Type[G] = TSet(TTuple(Seq(mapType.key, mapType.value)))

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc = assoc(map) <> ".items"
}