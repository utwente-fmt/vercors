package vct.col.ast.declaration.singular

import vct.col.ast.ParInvariantDecl
import vct.col.print.{Ctx, Doc, Text}

trait ParInvariantDeclImpl[G] {
  this: ParInvariantDecl[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(this))
}
