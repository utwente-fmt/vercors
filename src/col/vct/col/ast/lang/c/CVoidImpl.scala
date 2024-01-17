package vct.col.ast.lang.c

import vct.col.ast.CVoid
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CVoidOps

trait CVoidImpl[G] extends CVoidOps[G] { this: CVoid[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("void")
}