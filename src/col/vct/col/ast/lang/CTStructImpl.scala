package vct.col.ast.lang

import vct.col.ast.CTStruct
import vct.col.print.{Ctx, Doc, Text}

trait CTStructImpl[G] { this: CTStruct[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("struct") <+> ctx.name(ref)
}