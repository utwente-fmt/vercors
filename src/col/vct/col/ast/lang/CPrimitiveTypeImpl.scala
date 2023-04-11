package vct.col.ast.lang

import vct.col.ast.CPrimitiveType
import vct.col.print.{Ctx, Doc}

trait CPrimitiveTypeImpl[G] { this: CPrimitiveType[G] =>
  override def layout(implicit ctx: Ctx): Doc = Doc.spread(specifiers)
}