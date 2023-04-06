package vct.col.ast.`type`

import vct.col.ast.TSet
import vct.col.print.{Ctx, Doc, Group, Text}

trait TSetImpl[G] { this: TSet[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("seq") <> open <> Doc.arg(element) <> close)
}