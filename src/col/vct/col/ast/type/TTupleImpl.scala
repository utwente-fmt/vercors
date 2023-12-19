package vct.col.ast.`type`

import vct.col.ast.TTuple
import vct.col.print.{Ctx, Doc, Group, Text}

trait TTupleImpl[G] { this: TTuple[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("seq") <> open <> Doc.args(elements) <> close)
}