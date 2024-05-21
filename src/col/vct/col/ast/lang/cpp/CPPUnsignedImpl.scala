package vct.col.ast.lang.cpp

import vct.col.ast.CPPUnsigned
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CPPUnsignedOps

trait CPPUnsignedImpl[G] extends CPPUnsignedOps[G] { this: CPPUnsigned[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("unsigned")
}