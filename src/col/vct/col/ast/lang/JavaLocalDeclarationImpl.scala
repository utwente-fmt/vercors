package vct.col.ast.lang

import vct.col.ast.JavaLocalDeclaration
import vct.col.print.{Ctx, Doc, Group}

trait JavaLocalDeclarationImpl[G] {
  this: JavaLocalDeclaration[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Doc.rspread(modifiers) <> t <+> Doc.args(decls))
}
