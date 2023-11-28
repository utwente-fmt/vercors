package vct.col.ast.family.fieldflag

import vct.col.ast.FieldFlag
import vct.col.print._
import vct.col.ast.ops.FieldFlagFamilyOps

trait FieldFlagImpl[G] extends FieldFlagFamilyOps[G] { this: FieldFlag[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("final")
}
