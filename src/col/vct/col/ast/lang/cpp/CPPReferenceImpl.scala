package vct.col.ast.lang.cpp

import vct.col.ast.CPPReference
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CPPReferenceOps

trait CPPReferenceImpl[G] extends CPPReferenceOps[G] {
  this: CPPReference[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("&")
}
