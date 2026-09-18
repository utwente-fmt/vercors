package vct.col.ast.family.fieldflag

import vct.col.ast.Unique
import vct.col.ast.ops.UniqueOps
import vct.col.print.{Ctx, Doc, Text}

trait UniqueImpl[G] extends UniqueOps[G] {
  this: Unique[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text(s"unique<$unique>")
}
