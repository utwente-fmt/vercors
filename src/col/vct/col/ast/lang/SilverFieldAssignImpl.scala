package vct.col.ast.lang

import vct.col.ast.SilverFieldAssign
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait SilverFieldAssignImpl[G] { this: SilverFieldAssign[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    obj.bind(Precedence.POSTFIX) <> "." <> ctx.name(field) <+> ":=" <+> value
}