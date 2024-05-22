package vct.col.ast.lang.java

import vct.col.ast.JavaStrictFP
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.JavaStrictFPOps

trait JavaStrictFPImpl[G] extends JavaStrictFPOps[G] { this: JavaStrictFP[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("strictfp")
}