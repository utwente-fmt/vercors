package vct.col.ast.lang

import vct.col.ast.CPPGlobalDeclaration
import vct.col.print._

trait CPPGlobalDeclarationImpl[G] { this: CPPGlobalDeclaration[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    decl.show <> ";"
}