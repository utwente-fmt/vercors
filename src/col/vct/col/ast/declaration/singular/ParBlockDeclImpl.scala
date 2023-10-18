package vct.col.ast.declaration.singular

import vct.col.ast.ParBlockDecl
import vct.col.print._

trait ParBlockDeclImpl[G] {
  this: ParBlockDecl[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(this))
}
