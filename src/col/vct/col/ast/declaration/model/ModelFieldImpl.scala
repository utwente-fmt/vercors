package vct.col.ast.declaration.model

import vct.col.ast.ModelField
import vct.col.print._
import vct.col.ast.ops.ModelFieldOps

trait ModelFieldImpl[G] extends ModelFieldOps[G] {
  this: ModelField[G] =>
  override def layout(implicit ctx: Ctx): Doc = t.show <+> ctx.name(this) <> ";"
}
