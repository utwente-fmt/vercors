package vct.col.ast.`type`

import vct.col.ast.TAnyClass
import vct.col.print._

trait TAnyClassImpl[G] { this: TAnyClass[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("Object")
}
