package vct.col.ast.statement.exceptional

import vct.col.ast.Continue
import vct.col.print.{Ctx, Doc, Empty, Text}
import vct.col.ast.ops.ContinueOps

trait ContinueImpl[G] extends ContinueOps[G] {
  this: Continue[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("continue") <>
      (if (label.isEmpty)
         Empty
       else
         Empty <+> ctx.name(label.get)) <> ";"
}
