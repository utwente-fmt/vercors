package vct.col.ast.declaration.singular

import vct.col.ast.SendDecl
import vct.col.print.{Ctx, Doc, Text}

trait SendDeclImpl[G] { this: SendDecl[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text(ctx.name(this))
}
