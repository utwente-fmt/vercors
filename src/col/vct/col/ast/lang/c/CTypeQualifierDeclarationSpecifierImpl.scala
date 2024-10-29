package vct.col.ast.lang.c

import vct.col.ast.CTypeQualifierDeclarationSpecifier
import vct.col.print.{Ctx, Doc}
import vct.col.ast.ops.CTypeQualifierDeclarationSpecifierOps

trait CTypeQualifierDeclarationSpecifierImpl[G]
    extends CTypeQualifierDeclarationSpecifierOps[G] {
  this: CTypeQualifierDeclarationSpecifier[G] =>
  override def layout(implicit ctx: Ctx): Doc = typeQual.show
}
