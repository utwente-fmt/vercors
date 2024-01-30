package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait TLongImpl[G] {
  this: TLong[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("long")
}