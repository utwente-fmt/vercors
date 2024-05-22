package vct.col.ast.lang.silver

import vct.col.ast.{Expr, SilverLocalAssign}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.SilverLocalAssignOps

trait SilverLocalAssignImpl[G] extends SilverLocalAssignOps[G] { this: SilverLocalAssign[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text(ctx.name(v)) <+> ":=" <+> value

  override def expr: Expr[G] = this.value
}