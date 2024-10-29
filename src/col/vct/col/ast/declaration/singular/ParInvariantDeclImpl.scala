package vct.col.ast.declaration.singular

import vct.col.ast.ParInvariantDecl
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.{ParInvariantDeclOps, ParInvariantDeclFamilyOps}

trait ParInvariantDeclImpl[G]
    extends ParInvariantDeclOps[G] with ParInvariantDeclFamilyOps[G] {
  this: ParInvariantDecl[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(this))
}
