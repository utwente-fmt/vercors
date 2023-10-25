package vct.col.ast.lang

import vct.col.ast.CPPReference
import vct.col.print.{Ctx, Doc, Text}

trait CPPReferenceImpl[G] { this: CPPReference[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("&")
}