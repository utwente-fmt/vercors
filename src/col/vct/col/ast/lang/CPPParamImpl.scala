package vct.col.ast.lang

import vct.col.ast.CPPParam
import vct.col.print.{Ctx, Doc}

trait CPPParamImpl[G] {
  this: CPPParam[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.spread(specifiers) <+> declarator
}
