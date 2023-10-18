package vct.col.ast.statement.composite

import vct.col.ast.ParBarrier
import vct.col.print.{Ctx, Doc, Empty, Text}

trait ParBarrierImpl[G] {
  this: ParBarrier[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("barrier(") <> ctx.name(block) <>
      (if (invs.nonEmpty)
         Text(";") <+> Doc.args(invs.map(ctx.name).map(Text))
       else
         Empty) <> ")" <+> content.layoutAsBlock
}
