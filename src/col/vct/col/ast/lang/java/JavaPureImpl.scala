package vct.col.ast.lang.java

import vct.col.ast.JavaPure
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.JavaPureOps

trait JavaPureImpl[G] extends JavaPureOps[G] { this: JavaPure[G] =>
  override def layout(implicit ctx: Ctx): Doc = Doc.inlineSpec(Text("pure"))
}