package vct.col.ast.family.fieldflag

import vct.col.ast.FieldFlag
import vct.col.print._

trait FieldFlagImpl[G] {
  this: FieldFlag[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("final")
}
