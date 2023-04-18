package vct.col.ast.lang

import vct.col.ast.CLocalDeclaration
import vct.col.print.{Ctx, Doc, Text}

trait CLocalDeclarationImpl[G] { this: CLocalDeclaration[G] =>
  override def layout(implicit ctx: Ctx): Doc = decl.show <> ";"
}
