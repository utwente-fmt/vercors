package vct.col.ast.lang.java

import vct.col.ast.JavaLocalDeclarationStatement
import vct.col.print.{Ctx, Doc, Group}
import vct.col.ast.ops.JavaLocalDeclarationStatementOps

trait JavaLocalDeclarationStatementImpl[G] extends JavaLocalDeclarationStatementOps[G] { this: JavaLocalDeclarationStatement[G] =>
  override def layout(implicit ctx: Ctx): Doc = decl.show <> ";"
}