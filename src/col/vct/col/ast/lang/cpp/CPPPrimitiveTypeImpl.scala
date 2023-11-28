package vct.col.ast.lang.cpp

import vct.col.ast.CPPPrimitiveType
import vct.col.print.{Ctx, Doc}
import vct.col.ast.ops.CPPPrimitiveTypeOps

trait CPPPrimitiveTypeImpl[G] extends CPPPrimitiveTypeOps[G] { this: CPPPrimitiveType[G] =>
  override def layout(implicit ctx: Ctx): Doc = Doc.spread(specifiers)
}