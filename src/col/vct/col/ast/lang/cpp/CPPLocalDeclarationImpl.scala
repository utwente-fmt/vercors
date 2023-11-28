package vct.col.ast.lang.cpp

import vct.col.ast.CPPLocalDeclaration
import vct.col.print.{Ctx, Doc}
import vct.col.ast.ops.{CPPLocalDeclarationOps, CPPLocalDeclarationFamilyOps}

trait CPPLocalDeclarationImpl[G] extends CPPLocalDeclarationOps[G] with CPPLocalDeclarationFamilyOps[G] { this: CPPLocalDeclaration[G] =>
  override def layout(implicit ctx: Ctx): Doc = decl.show
}
