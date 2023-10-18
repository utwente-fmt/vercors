package vct.col.ast.declaration.global

import vct.col.ast.Model
import vct.col.print._

trait ModelImpl[G] {
  this: Model[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("model") <+> ctx.name(this) <+> "{" <+/> Doc.stack(declarations) <+/>
      "}"
}
