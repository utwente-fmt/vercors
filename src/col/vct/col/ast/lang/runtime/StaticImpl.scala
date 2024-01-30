package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._

trait StaticImpl[G] {
  this: Static[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("static")
}