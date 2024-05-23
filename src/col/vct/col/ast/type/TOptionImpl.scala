package vct.col.ast.`type`

import vct.col.ast.TOption
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.TOptionOps

trait TOptionImpl[G] extends TOptionOps[G] {
  this: TOption[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("option") <> open <> Doc.arg(element) <> close)
}
