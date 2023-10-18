package vct.col.ast.lang

import vct.col.ast.CPPLocalDeclaration
import vct.col.print.{Ctx, Doc}

trait CPPLocalDeclarationImpl[G] {
  this: CPPLocalDeclaration[G] =>
  override def layout(implicit ctx: Ctx): Doc = decl.show
}
