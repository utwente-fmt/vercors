package vct.col.ast.lang.c

import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.CDouble

trait CDoubleImpl[G] { this: CDouble[G] =>
   override def layout(implicit ctx: Ctx): Doc = Text("double")
}