package vct.col.ast.lang.c

import vct.col.ast.CPrimitiveType
import vct.col.print.{Ctx, Doc}
import vct.col.ast.ops.CPrimitiveTypeOps

trait CPrimitiveTypeImpl[G] extends CPrimitiveTypeOps[G] { this: CPrimitiveType[G] =>
  override def layout(implicit ctx: Ctx): Doc = Doc.spread(specifiers)
}