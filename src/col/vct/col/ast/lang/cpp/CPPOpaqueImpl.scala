package vct.col.ast.lang.cpp

import vct.col.ast.CPPOpaque
import vct.col.ast.ops.CPPOpaqueOps
import vct.col.print.{Ctx, Doc, Text}

trait CPPOpaqueImpl[G] extends CPPOpaqueOps[G] {
  this: CPPOpaque[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("opaque")
}
