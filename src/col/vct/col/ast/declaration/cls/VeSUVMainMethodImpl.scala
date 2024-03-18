package vct.col.ast.declaration.cls

import vct.col.ast.VeSUVMainMethod
import vct.col.print.{Ctx, Doc, Empty, Text}
import vct.col.ast.ops.VeSUVMainMethodOps

trait VeSUVMainMethodImpl[G] extends VeSUVMainMethodOps[G] { this: VeSUVMainMethod[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      Text("vesuv_entry") <> body.map(Empty <+> _.layoutAsBlock).getOrElse(Text(";")),
    ))
}
