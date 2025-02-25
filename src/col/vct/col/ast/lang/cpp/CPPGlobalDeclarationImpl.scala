package vct.col.ast.lang.cpp

import vct.col.ast.CPPGlobalDeclaration
import vct.col.print._
import vct.col.ast.ops.CPPGlobalDeclarationOps

trait CPPGlobalDeclarationImpl[G] extends CPPGlobalDeclarationOps[G] {
  this: CPPGlobalDeclaration[G] =>
  override def layout(implicit ctx: Ctx): Doc = decl.show <> ";"
}
