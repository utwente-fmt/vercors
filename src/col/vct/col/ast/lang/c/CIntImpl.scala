package vct.col.ast.lang.c

import vct.col.ast.CInt
import vct.col.ast.ops.CIntOps
import vct.col.print.{Ctx, Doc, Text}

trait CIntImpl[G] extends CIntOps[G] { this: CInt[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("int")
}