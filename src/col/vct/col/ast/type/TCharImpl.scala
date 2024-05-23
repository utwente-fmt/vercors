package vct.col.ast.`type`

import vct.col.ast.TChar
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.TCharOps

trait TCharImpl[G] extends TCharOps[G] {
  this: TChar[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("char")
}
