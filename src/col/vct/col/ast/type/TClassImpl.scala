package vct.col.ast.`type`

import vct.col.ast.{Class, TClass}
import vct.col.print.{Ctx, Doc, Empty, Group, Text}

trait TClassImpl[G] { this: TClass[G] =>
  def transSupportArrows: Seq[(Class[G], Class[G])] = cls.decl.transSupportArrows

  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(cls))
}