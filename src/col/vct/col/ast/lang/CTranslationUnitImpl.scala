package vct.col.ast.lang

import vct.col.ast.CTranslationUnit
import vct.col.print.{Ctx, Doc, Group}

trait CTranslationUnitImpl[G] { this: CTranslationUnit[G] =>
  override def layout(implicit ctx: Ctx): Doc = Doc.stack(declarations)
}
