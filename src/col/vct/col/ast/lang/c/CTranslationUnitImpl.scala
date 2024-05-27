package vct.col.ast.lang.c

import vct.col.ast.CTranslationUnit
import vct.col.print.{Ctx, Doc, Group}
import vct.col.ast.ops.CTranslationUnitOps

trait CTranslationUnitImpl[G] extends CTranslationUnitOps[G] {
  this: CTranslationUnit[G] =>
  override def layout(implicit ctx: Ctx): Doc = Doc.stack(declarations)
}
