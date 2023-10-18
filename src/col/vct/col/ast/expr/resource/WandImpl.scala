package vct.col.ast.expr.resource

import vct.col.ast.{TResource, Type, Wand}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait WandImpl[G] {
  this: Wand[G] =>
  override def t: Type[G] = TResource()

  override def precedence: Int = Precedence.IMPLIES
  override def layout(implicit ctx: Ctx): Doc =
    rassoc(
      left,
      if (ctx.syntax == Ctx.Silver)
        "--*"
      else
        "-*",
      right,
    )
}
