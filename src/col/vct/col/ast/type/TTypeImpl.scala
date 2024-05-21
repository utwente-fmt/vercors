package vct.col.ast.`type`

import vct.col.ast.TType
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.TTypeOps

trait TTypeImpl[G] extends TTypeOps[G] { this: TType[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("type") <> open <> Doc.arg(t) <> close)
}