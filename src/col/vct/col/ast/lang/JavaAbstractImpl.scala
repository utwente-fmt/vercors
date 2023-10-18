package vct.col.ast.lang

import vct.col.ast.JavaAbstract
import vct.col.print._

trait JavaAbstractImpl[G] {
  this: JavaAbstract[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("abstract")
}
