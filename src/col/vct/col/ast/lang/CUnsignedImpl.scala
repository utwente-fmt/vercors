package vct.col.ast.lang

import vct.col.ast.CUnsigned
import vct.col.print.{Ctx, Doc, Text}

trait CUnsignedImpl[G] {
  this: CUnsigned[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("unsigned")
}
