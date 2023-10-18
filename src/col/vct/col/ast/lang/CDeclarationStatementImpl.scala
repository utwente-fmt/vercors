package vct.col.ast.lang

import vct.col.ast.CDeclarationStatement
import vct.col.print.{Ctx, Doc, Text}

trait CDeclarationStatementImpl[G] {
  this: CDeclarationStatement[G] =>
  override def layout(implicit ctx: Ctx): Doc = decl.show <> ";"
}
