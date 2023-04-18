package vct.col.ast.expr.sideeffect

import vct.col.ast.{Type, With}
import vct.col.print.{Ctx, Doc, Group, Precedence, Show, Text}

trait WithImpl[G] { this: With[G] =>
  override def t: Type[G] = value.t

  def layoutEffect(implicit ctx: Ctx): Doc =
    Group(Text("with") <+> pre.layoutAsBlock)

  override def precedence: Int = Precedence.PVL_WITH_THEN
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(value) <>> Doc.inlineSpec(Show.lazily(layoutEffect(_))))
}