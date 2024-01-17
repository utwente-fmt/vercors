package vct.col.ast.declaration.global

import vct.col.ast.PVLSeqProg
import vct.col.print._
import vct.col.ast.ops.PVLSeqProgOps

trait PVLSeqProgImpl[G] extends PVLSeqProgOps[G] { this: PVLSeqProg[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(Text("seq_program") <+> ctx.name(this) <> "(" <> Doc.args(args) <> ")") <+> "{" <>>
        Doc.stack(declarations) <+/>
        "}"
    ))
}
