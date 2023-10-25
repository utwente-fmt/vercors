package vct.col.ast.lang

import vct.col.ast.CPPVoid
import vct.col.print.{Ctx, Doc, Text}

trait CPPVoidImpl[G] { this: CPPVoid[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("void")
}