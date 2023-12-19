package vct.col.ast.`type`

import vct.col.ast.TVoid
import vct.col.print.{Ctx, Doc, Text}

trait TVoidImpl[G] { this: TVoid[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("void")
}