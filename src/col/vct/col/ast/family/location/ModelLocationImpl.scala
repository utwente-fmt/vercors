package vct.col.ast.family.location

import vct.col.ast.ModelLocation
import vct.col.print.{Ctx, Doc, Precedence}
import vct.col.ast.ops.ModelLocationOps

trait ModelLocationImpl[G] extends ModelLocationOps[G] { this: ModelLocation[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    obj.bind(Precedence.POSTFIX) <> "." <> ctx.name(field)
}
