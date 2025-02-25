package vct.col.ast.family.bipporttype

import vct.col.ast.BipSpontaneous
import vct.col.print._
import vct.col.ast.ops.BipSpontaneousOps

trait BipSpontaneousImpl[G] extends BipSpontaneousOps[G] {
  this: BipSpontaneous[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("spontaneous")
}
