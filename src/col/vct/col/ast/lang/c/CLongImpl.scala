package vct.col.ast.lang.c

import vct.col.ast.CLong
import vct.col.ast.ops.CLongOps
import vct.col.print.{Ctx, Doc, Text}

trait CLongImpl[G] extends CLongOps[G] { this: CLong[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("long")
}