package vct.col.ast.statement.veymont

import vct.col.ast.PVLSeqAssign
import vct.col.print.{Ctx, Doc, Group, Text}

trait  PVLSeqAssignImpl[G] { this: PVLSeqAssign[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text(receiver.decl.name) <> "." <> ctx.name(field) <+> ":=" <+> value.show)
}
