package vct.col.ast.lang.c

import vct.col.ast.CTEnum
import vct.col.ast.ops.CTEnumOps
import vct.col.print.{Ctx, Doc, Text}
import vct.col.typerules.TypeSize

trait CTEnumImpl[G] extends CTEnumOps[G] {
  this: CTEnum[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("enum") <+> ctx.name(ref)

  override def bits: TypeSize = TypeSize.Minimally(BigInt(8))
}
