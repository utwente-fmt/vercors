package vct.col.ast.`type`

import vct.col.ast.TCInt
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.TCIntOps

trait TCIntImpl[G] extends TCIntOps[G] { this: TCInt[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("int")
}