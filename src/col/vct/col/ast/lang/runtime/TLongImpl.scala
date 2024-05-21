package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.TLongOps

trait TLongImpl[G] extends TLongOps[G] {
  this: TLong[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("long")
}