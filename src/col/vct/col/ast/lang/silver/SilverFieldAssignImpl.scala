package vct.col.ast.lang.silver

import vct.col.ast.SilverFieldAssign
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.SilverFieldAssignOps

trait SilverFieldAssignImpl[G] extends SilverFieldAssignOps[G] { this: SilverFieldAssign[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    obj.bind(Precedence.POSTFIX) <> "." <> ctx.name(field) <+> ":=" <+> value
}