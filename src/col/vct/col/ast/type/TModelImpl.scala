package vct.col.ast.`type`

import vct.col.ast.TModel
import vct.col.print._
import vct.col.ast.ops.TModelOps

trait TModelImpl[G] extends TModelOps[G] {
  this: TModel[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(model))
}
