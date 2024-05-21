package vct.col.ast.family.bipporttype

import vct.col.ast.BipEnforceable
import vct.col.print._
import vct.col.ast.ops.BipEnforceableOps

trait BipEnforceableImpl[G] extends BipEnforceableOps[G] { this: BipEnforceable[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("enforceable")
}
