package vct.col.ast.expr.op.map

import vct.col.ast.{MapCons, TMap, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence}

trait MapConsImpl[G] { this: MapCons[G] =>
  override def t: TMap[G] = tailType
  def tailType: TMap[G] = map.t.asMap.get

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(map) <> ".add(" <> Doc.args(Seq(k, v)) <> ")")
}