package vct.col.ast.declaration.global

import vct.col.ast.Model
import vct.col.print._
import vct.col.ast.ops.ModelOps

trait ModelImpl[G] extends ModelOps[G] {
  this: Model[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("model") <+> ctx.name(this) <+> "{" <+/> Doc.stack(declarations) <+/>
      "}"
}
