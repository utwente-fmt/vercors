package vct.col.ast.`type`

import vct.col.ast.TAnyClass
import vct.col.print._
import vct.col.ast.ops.TAnyClassOps

trait TAnyClassImpl[G] extends TAnyClassOps[G] { this: TAnyClass[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("Object")
}
