package vct.col.ast.lang.runtime

import vct.col.ast._
import vct.col.print._
import vct.col.ast.ops.StaticOps

trait StaticImpl[G] extends StaticOps[G] {
  this: Static[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("static")
}