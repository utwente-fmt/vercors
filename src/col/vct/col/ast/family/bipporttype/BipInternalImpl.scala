package vct.col.ast.family.bipporttype

import vct.col.ast.BipInternal
import vct.col.print._
import vct.col.ast.ops.BipInternalOps

trait BipInternalImpl[G] extends BipInternalOps[G] { this: BipInternal[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("internal")
}
