package vct.col.ast.lang

import vct.col.ast.CPPDeclarationStatement
import vct.col.print.{Ctx, Doc}

trait CPPDeclarationStatementImpl[G] {
  this: CPPDeclarationStatement[G] =>
  override def layout(implicit ctx: Ctx): Doc = decl.show <> ";"
}
