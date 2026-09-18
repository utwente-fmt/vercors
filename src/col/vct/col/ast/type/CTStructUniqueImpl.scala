package vct.col.ast.`type`

import vct.col.ast.CTStructUnique
import vct.col.ast.ops.CTStructUniqueOps
import vct.col.print._

trait CTStructUniqueImpl[G] extends CTStructUniqueOps[G] { this: CTStructUnique[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("unique_pointer_field<") <>
    ctx.name(pointerFieldRef) <> "," <> unique.toString <> ">" <+> inner
}
