package vct.col.ast.lang.c

import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.CFloat

trait CFloatImpl[G] { this: CFloat[G] =>
   override def layout(implicit ctx: Ctx): Doc = Text("float")
}