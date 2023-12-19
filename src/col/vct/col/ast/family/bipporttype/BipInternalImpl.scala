package vct.col.ast.family.bipporttype

import vct.col.ast.BipInternal
import vct.col.print._

trait BipInternalImpl[G] { this: BipInternal[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("internal")
}
