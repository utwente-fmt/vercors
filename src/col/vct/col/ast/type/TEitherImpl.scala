package vct.col.ast.`type`

import vct.col.ast.TEither
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.TEitherOps

trait TEitherImpl[G] extends TEitherOps[G] { this: TEither[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("either") <> open <> Doc.args(Seq(left, right)) <> close)
}