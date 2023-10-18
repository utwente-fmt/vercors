package vct.col.ast.`type`

import vct.col.ast.TModel
import vct.col.print._

trait TModelImpl[G] {
  this: TModel[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(model))
}
