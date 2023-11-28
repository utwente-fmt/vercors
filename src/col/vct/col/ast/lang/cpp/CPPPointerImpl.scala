package vct.col.ast.lang.cpp

import vct.col.ast.CPPPointer
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CPPPointerOps

trait CPPPointerImpl[G] extends CPPPointerOps[G] { this: CPPPointer[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("*")
}