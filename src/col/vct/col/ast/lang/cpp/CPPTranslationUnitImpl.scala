package vct.col.ast.lang.cpp

import vct.col.ast.CPPTranslationUnit
import vct.col.print.{Ctx, Doc}
import vct.col.ast.ops.CPPTranslationUnitOps

trait CPPTranslationUnitImpl[G] extends CPPTranslationUnitOps[G] { this: CPPTranslationUnit[G] =>
  override def layout(implicit ctx: Ctx): Doc = Doc.stack(declarations)
}
