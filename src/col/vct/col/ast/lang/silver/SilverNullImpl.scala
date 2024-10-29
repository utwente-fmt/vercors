package vct.col.ast.lang.silver

import vct.col.ast.{SilverNull, TRef}
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.ast.ops.SilverNullOps

trait SilverNullImpl[G] extends SilverNullOps[G] {
  this: SilverNull[G] =>
  override def t: TRef[G] = TRef[G]()

  override def layout(implicit ctx: Ctx): Doc = Text("null")
}
