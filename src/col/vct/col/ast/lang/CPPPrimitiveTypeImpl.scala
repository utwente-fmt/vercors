package vct.col.ast.lang

import vct.col.ast.CPPPrimitiveType
import vct.col.print.{Ctx, Doc}

trait CPPPrimitiveTypeImpl[G] { this: CPPPrimitiveType[G] =>
  override def layout(implicit ctx: Ctx): Doc = Doc.spread(specifiers)
}