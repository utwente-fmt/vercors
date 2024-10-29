package vct.col.ast.lang.cpp

import vct.col.ast.CPPLong
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CPPLongOps

trait CPPLongImpl[G] extends CPPLongOps[G] {
  this: CPPLong[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("long")
}
