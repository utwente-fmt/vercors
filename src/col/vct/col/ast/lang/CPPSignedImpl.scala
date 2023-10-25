package vct.col.ast.lang

import vct.col.ast.CPPSigned
import vct.col.print.{Ctx, Doc, Text}

trait CPPSignedImpl[G] { this: CPPSigned[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("signed")
}