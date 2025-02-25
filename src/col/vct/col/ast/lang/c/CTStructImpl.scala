package vct.col.ast.lang.c

import vct.col.ast.CTStruct
import vct.col.ast.ops.CTStructOps
import vct.col.print.{Ctx, Doc, Text}

trait CTStructImpl[G] extends CTStructOps[G] {
  this: CTStruct[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("struct") <+> ctx.name(ref)
}
