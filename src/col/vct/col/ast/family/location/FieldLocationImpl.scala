package vct.col.ast.family.location

import vct.col.ast.FieldLocation
import vct.col.print._
import vct.col.ast.ops.FieldLocationOps

trait FieldLocationImpl[G] extends FieldLocationOps[G] { this: FieldLocation[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    obj.bind(Precedence.POSTFIX) <> "." <> ctx.name(field)
}
