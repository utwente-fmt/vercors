package vct.col.ast.lang

import vct.col.ast.CPPUnsigned
import vct.col.print.{Ctx, Doc, Text}

trait CPPUnsignedImpl[G] { this: CPPUnsigned[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("unsigned")
}