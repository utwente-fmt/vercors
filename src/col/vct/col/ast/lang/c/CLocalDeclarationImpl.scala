package vct.col.ast.lang.c

import vct.col.ast.CLocalDeclaration
import vct.col.ast.ops.{CLocalDeclarationFamilyOps, CLocalDeclarationOps}
import vct.col.print.{Ctx, Doc}

trait CLocalDeclarationImpl[G] extends CLocalDeclarationOps[G] with CLocalDeclarationFamilyOps[G] { this: CLocalDeclaration[G] =>
  override def layout(implicit ctx: Ctx): Doc = decl.show <> ";"
}
