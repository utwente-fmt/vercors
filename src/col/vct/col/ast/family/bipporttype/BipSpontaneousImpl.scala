package vct.col.ast.family.bipporttype

import vct.col.ast.BipSpontaneous
import vct.col.print._

trait BipSpontaneousImpl[G] { this: BipSpontaneous[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("spontaneous")
}
