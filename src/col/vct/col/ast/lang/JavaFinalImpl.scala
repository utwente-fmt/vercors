package vct.col.ast.lang

import vct.col.ast.JavaFinal
import vct.col.print.{Ctx, Doc, Text}

trait JavaFinalImpl[G] {
  this: JavaFinal[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("final")
}
