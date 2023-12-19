package vct.col.ast.declaration.singular

import vct.col.ast.ParBlockDecl
import vct.col.print._
import vct.col.ast.ops.{ParBlockDeclOps, ParBlockDeclFamilyOps}

trait ParBlockDeclImpl[G] extends ParBlockDeclOps[G] with ParBlockDeclFamilyOps[G] { this: ParBlockDecl[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(this))
}