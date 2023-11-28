package vct.col.ast.lang.java

import vct.col.ast.JavaFinal
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.JavaFinalOps

trait JavaFinalImpl[G] extends JavaFinalOps[G] { this: JavaFinal[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("final")
}