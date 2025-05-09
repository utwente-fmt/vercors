package vct.col.ast.lang.c

import vct.col.ast.{CPrimitiveType, CUnsigned}
import vct.col.print.{Ctx, Doc}
import vct.col.ast.ops.CPrimitiveTypeOps
import vct.col.resolve.lang.C
import vct.col.typerules.TypeSize

trait CPrimitiveTypeImpl[G] extends CPrimitiveTypeOps[G] {
  this: CPrimitiveType[G] =>
  override def signed: Boolean = C.isSigned(specifiers)

  override def bits: TypeSize = C.stripCPrimitiveType(this).bits
  override def layout(implicit ctx: Ctx): Doc = Doc.spread(specifiers)
}
