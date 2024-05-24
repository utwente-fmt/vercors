package vct.col.ast.declaration.global

import vct.col.ast.PVLChoreography
import vct.col.print._
import vct.col.ast.ops.PVLChoreographyOps

trait PVLChoreographyImpl[G] extends PVLChoreographyOps[G] { this: PVLChoreography[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract,
      Group(Text("seq_program") <+> ctx.name(this) <> "(" <> Doc.args(args) <> ")") <+> "{" <>>
        Doc.stack(declarations) <+/>
        "}"
    ))
}
