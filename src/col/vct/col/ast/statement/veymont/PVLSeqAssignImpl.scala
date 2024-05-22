package vct.col.ast.statement.veymont

import vct.col.ast.{Expr, PVLSeqAssign}
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.PVLSeqAssignOps

trait  PVLSeqAssignImpl[G] extends PVLSeqAssignOps[G] { this: PVLSeqAssign[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text(receiver.decl.name) <> "." <> ctx.name(field) <+> ":=" <+> value.show)

  override def expr: Expr[G] = this.value
}
