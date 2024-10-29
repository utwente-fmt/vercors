package vct.col.ast.declaration.singular

import vct.col.ast.SendDecl
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.{SendDeclOps, SendDeclFamilyOps}

trait SendDeclImpl[G] extends SendDeclOps[G] with SendDeclFamilyOps[G] {
  this: SendDecl[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(this))
}
