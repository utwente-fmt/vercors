package vct.col.ast.`type`

import vct.col.ast.TMap
import vct.col.print.{Ctx, Doc, Group, Text}

trait TMapImpl[G] {
  this: TMap[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("maps") <> open <> Doc.args(Seq(key, value)) <> close)
}
