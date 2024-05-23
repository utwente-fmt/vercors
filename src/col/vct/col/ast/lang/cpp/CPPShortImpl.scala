package vct.col.ast.lang.cpp

import vct.col.ast.CPPShort
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CPPShortOps

trait CPPShortImpl[G] extends CPPShortOps[G] { this: CPPShort[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("short")
}