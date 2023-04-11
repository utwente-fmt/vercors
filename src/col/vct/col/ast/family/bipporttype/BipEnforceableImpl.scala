package vct.col.ast.family.bipporttype

import vct.col.ast.BipEnforceable
import vct.col.print._

trait BipEnforceableImpl[G] { this: BipEnforceable[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("enforceable")
}
