package vct.col.ast.`type`

import vct.col.ast.TSeqProg
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.TSeqProgOps

trait TSeqProgImpl[G] extends TSeqProgOps[G] { this: TSeqProg[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(cls))
}
