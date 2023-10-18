package vct.col.ast.lang

import vct.col.ast.CPPTranslationUnit
import vct.col.print.{Ctx, Doc}

trait CPPTranslationUnitImpl[G] {
  this: CPPTranslationUnit[G] =>
  override def layout(implicit ctx: Ctx): Doc = Doc.stack(declarations)
}
