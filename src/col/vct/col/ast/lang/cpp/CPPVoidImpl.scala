package vct.col.ast.lang.cpp

import vct.col.ast.CPPVoid
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CPPVoidOps

trait CPPVoidImpl[G] extends CPPVoidOps[G] { this: CPPVoid[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("void")
}