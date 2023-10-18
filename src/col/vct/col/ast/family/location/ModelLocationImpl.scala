package vct.col.ast.family.location

import vct.col.ast.ModelLocation
import vct.col.print.{Ctx, Doc, Precedence}

trait ModelLocationImpl[G] {
  this: ModelLocation[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    obj.bind(Precedence.POSTFIX) <> "." <> ctx.name(field)
}
