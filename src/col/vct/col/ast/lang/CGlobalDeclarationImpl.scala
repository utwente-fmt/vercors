package vct.col.ast.lang

import vct.col.ast.CGlobalDeclaration
import vct.col.print._

trait CGlobalDeclarationImpl[G] {
  this: CGlobalDeclaration[G] =>
  override def layout(implicit ctx: Ctx): Doc = decl.show <> ";"
}
