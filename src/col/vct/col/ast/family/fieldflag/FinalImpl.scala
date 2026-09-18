package vct.col.ast.family.fieldflag

import vct.col.ast.Final
import vct.col.ast.ops.FinalOps
import vct.col.print.{Ctx, Doc, Text}

trait FinalImpl[G] extends FinalOps[G] {
  this: Final[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("final")
}
