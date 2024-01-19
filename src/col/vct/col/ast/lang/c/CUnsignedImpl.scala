package vct.col.ast.lang.c

import vct.col.ast.CUnsigned
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CUnsignedOps

trait CUnsignedImpl[G] extends CUnsignedOps[G] { this: CUnsigned[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("unsigned")
}