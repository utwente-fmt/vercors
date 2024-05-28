package vct.col.ast.declaration.singular

import vct.col.ast.LabelDecl
import vct.col.print._
import vct.col.ast.ops.{LabelDeclOps, LabelDeclFamilyOps}

trait LabelDeclImpl[G] extends LabelDeclOps[G] with LabelDeclFamilyOps[G] {
  this: LabelDecl[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(this))
}
