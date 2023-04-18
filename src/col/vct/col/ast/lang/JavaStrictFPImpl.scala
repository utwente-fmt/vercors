package vct.col.ast.lang

import vct.col.ast.JavaStrictFP
import vct.col.print.{Ctx, Doc, Text}

trait JavaStrictFPImpl[G] { this: JavaStrictFP[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("strictfp")
}