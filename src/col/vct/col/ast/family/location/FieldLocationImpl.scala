package vct.col.ast.family.location

import vct.col.ast.FieldLocation
import vct.col.print._

trait FieldLocationImpl[G] {
  this: FieldLocation[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    obj.bind(Precedence.POSTFIX) <> "." <> ctx.name(field)
}
