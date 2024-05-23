package vct.col.ast.lang.c

import vct.col.ast.CStatic
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.CStaticOps

trait CStaticImpl[G] extends CStaticOps[G] { this: CStatic[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("static")
}