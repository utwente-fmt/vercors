package vct.col.ast.lang

import vct.col.ast.JavaLocalDeclarationStatement
import vct.col.print.{Ctx, Doc, Group}

trait JavaLocalDeclarationStatementImpl[G] {
  this: JavaLocalDeclarationStatement[G] =>
  override def layout(implicit ctx: Ctx): Doc = decl.show <> ";"
}
