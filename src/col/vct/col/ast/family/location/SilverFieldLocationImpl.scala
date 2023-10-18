package vct.col.ast.family.location

import vct.col.ast.SilverFieldLocation
import vct.col.print.{Ctx, Doc, Precedence}

trait SilverFieldLocationImpl[G] {
  this: SilverFieldLocation[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    obj.bind(Precedence.POSTFIX) <> "." <> ctx.name(field)
}
