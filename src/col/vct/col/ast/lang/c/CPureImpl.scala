package vct.col.ast.lang.c

import vct.col.ast.CPure
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CPureOps

trait CPureImpl[G] extends CPureOps[G] { this: CPure[G] =>
  override def layout(implicit ctx: Ctx): Doc = Doc.inlineSpec(Text("pure"))
}