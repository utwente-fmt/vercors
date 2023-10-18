package vct.col.ast.declaration.singular

import vct.col.ast.LabelDecl
import vct.col.print._

trait LabelDeclImpl[G] {
  this: LabelDecl[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(this))
}
