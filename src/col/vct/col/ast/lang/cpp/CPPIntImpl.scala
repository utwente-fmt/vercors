package vct.col.ast.lang.cpp

import vct.col.ast.CPPInt
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CPPIntOps

trait CPPIntImpl[G] extends CPPIntOps[G] {
  this: CPPInt[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("int")
}
