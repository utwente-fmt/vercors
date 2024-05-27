package vct.col.ast.lang.silver

import vct.col.ast.SilverField
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.SilverFieldOps

trait SilverFieldImpl[G] extends SilverFieldOps[G] {
  this: SilverField[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("field") <+> ctx.name(this) <> ":" <+> t
}
