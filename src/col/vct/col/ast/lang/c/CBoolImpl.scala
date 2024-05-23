package vct.col.ast.lang.c

import vct.col.ast.CBool
import vct.col.ast.ops.CBoolOps
import vct.col.print.{Ctx, Doc, Text}

trait CBoolImpl[G] extends CBoolOps[G] { this: CBool[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("_Bool")
}