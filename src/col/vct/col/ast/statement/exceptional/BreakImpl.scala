package vct.col.ast.statement.exceptional

import vct.col.ast.Break
import vct.col.print.{Ctx, Doc, Empty, Text}
import vct.col.ast.ops.BreakOps

trait BreakImpl[G] extends BreakOps[G] { this: Break[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("break") <> (if(label.isEmpty) Empty else Empty <+> ctx.name(label.get)) <> ";"
}