package vct.col.ast.lang.cpp

import vct.col.ast.CPPChar
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CPPCharOps

trait CPPCharImpl[G] extends CPPCharOps[G] {
  this: CPPChar[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("char")
}
