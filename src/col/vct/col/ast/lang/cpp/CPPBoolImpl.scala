package vct.col.ast.lang.cpp

import vct.col.ast.CPPBool
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CPPBoolOps

trait CPPBoolImpl[G] extends CPPBoolOps[G] {
  this: CPPBool[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("bool")
}
