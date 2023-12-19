package vct.col.ast.declaration.global

import vct.col.ast.PVLSeqProg
import vct.col.print._

trait PVLSeqProgImpl[G] { this: PVLSeqProg[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(Text("seq_program") <+> ctx.name(this) <> "(" <> Doc.args(args) <> ")") <+> "{" <>>
        Doc.stack(declarations) <+/>
        "}"
    ))
}
