package vct.col.ast.lang.c

import vct.col.ast.CDeclarationStatement
import vct.col.ast.ops.CDeclarationStatementOps
import vct.col.print.{Ctx, Doc}

trait CDeclarationStatementImpl[G] extends CDeclarationStatementOps[G] {
  this: CDeclarationStatement[G] =>
  override def layout(implicit ctx: Ctx): Doc = decl.show <> ";"
}
