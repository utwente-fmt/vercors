package vct.col.ast.lang

import vct.col.ast.CTypeQualifierDeclarationSpecifier
import vct.col.print.{Ctx, Doc}

trait CTypeQualifierDeclarationSpecifierImpl[G] { this: CTypeQualifierDeclarationSpecifier[G] =>
  override def layout(implicit ctx: Ctx): Doc = typeQual.show
}