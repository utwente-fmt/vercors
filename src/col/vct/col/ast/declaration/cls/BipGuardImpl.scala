package vct.col.ast.declaration.cls

import vct.col.ast.BipGuard
import vct.col.print.{Ctx, Doc, Group, Text, Empty}
import vct.col.ast.ops.BipGuardOps

trait BipGuardImpl[G] extends BipGuardOps[G] { this: BipGuard[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      if(pure) Text("@Pure") else Empty,
      Text("@Guard(name =") <+> ctx.name(this) <> ")",
      Text("public boolean") <+> ctx.name(this) <> "()" <+> body.layoutAsBlock
    ))
}
