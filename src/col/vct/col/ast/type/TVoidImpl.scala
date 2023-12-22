package vct.col.ast.`type`

import vct.col.ast.TVoid
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.TVoidOps

trait TVoidImpl[G] extends TVoidOps[G] { this: TVoid[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("void")
}