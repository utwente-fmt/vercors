package vct.col.ast.lang.cpp

import vct.col.ast.CPPPure
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CPPPureOps

trait CPPPureImpl[G] extends CPPPureOps[G] {
  this: CPPPure[G] =>
  override def layout(implicit ctx: Ctx): Doc = Doc.inlineSpec(Text("pure"))
}
