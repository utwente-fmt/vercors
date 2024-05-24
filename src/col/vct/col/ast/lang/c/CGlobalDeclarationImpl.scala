package vct.col.ast.lang.c

import vct.col.ast.CGlobalDeclaration
import vct.col.ast.ops.CGlobalDeclarationOps
import vct.col.print._

trait CGlobalDeclarationImpl[G] extends CGlobalDeclarationOps[G] { this: CGlobalDeclaration[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    decl.show <> ";"
}