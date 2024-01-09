package vct.col.ast.expr.sideeffect

import vct.col.ast.{Then, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Show, Text}
import vct.col.ast.ops.ThenOps

trait ThenImpl[G] extends ThenOps[G] { this: Then[G] =>
  override def t: Type[G] = value.t

  def layoutEffect(implicit ctx: Ctx): Doc =
    Group(Text("then") <+> post.layoutAsBlock)

  override def precedence: Int = Precedence.PVL_WITH_THEN
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(value) <>> Doc.inlineSpec(Show.lazily(layoutEffect(_))))
}