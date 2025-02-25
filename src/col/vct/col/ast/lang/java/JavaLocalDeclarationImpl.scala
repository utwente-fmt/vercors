package vct.col.ast.lang.java

import vct.col.ast.JavaLocalDeclaration
import vct.col.print.{Ctx, Doc, Group}
import vct.col.ast.ops.{JavaLocalDeclarationOps, JavaLocalDeclarationFamilyOps}

trait JavaLocalDeclarationImpl[G]
    extends JavaLocalDeclarationOps[G] with JavaLocalDeclarationFamilyOps[G] {
  this: JavaLocalDeclaration[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Doc.rspread(modifiers) <> t <+> Doc.args(decls))
}
