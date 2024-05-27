package vct.col.ast.`type`

import vct.col.ast.TVector
import vct.col.ast.ops.TVectorOps
import vct.col.print.{Ctx, Doc, Group, Text}

trait TVectorImpl[G] extends TVectorOps[G] { this: TVector[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("vector") <> open <> Doc.arg(element) <> "," <> size.toString <> close)
}