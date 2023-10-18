package vct.col.ast.lang

import vct.col.ast.CParam
import vct.col.print.{Ctx, Doc, Group, Text}

trait CParamImpl[G] {
  this: CParam[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.spread(specifiers) <+> declarator
}
