package vct.col.ast.`type`

import vct.col.ast.TSeq
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.TSeqOps

trait TSeqImpl[G] extends TSeqOps[G] { this: TSeq[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("seq") <> open <> Doc.arg(element) <> close)
}