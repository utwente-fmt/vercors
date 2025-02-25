package vct.col.ast.lang.cpp

import vct.col.ast.CPPDeclarationStatement
import vct.col.print.{Ctx, Doc}
import vct.col.ast.ops.CPPDeclarationStatementOps

trait CPPDeclarationStatementImpl[G] extends CPPDeclarationStatementOps[G] {
  this: CPPDeclarationStatement[G] =>
  override def layout(implicit ctx: Ctx): Doc = decl.show <> ";"
}
