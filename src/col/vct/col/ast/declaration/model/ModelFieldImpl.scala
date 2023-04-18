package vct.col.ast.declaration.model

import vct.col.ast.ModelField
import vct.col.print._

trait ModelFieldImpl[G] { this: ModelField[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    t.show <+> ctx.name(this) <> ";"
}