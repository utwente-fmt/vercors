package vct.col.ast.lang.java

import vct.col.ast.JavaAbstract
import vct.col.print._
import vct.col.ast.ops.JavaAbstractOps

trait JavaAbstractImpl[G] extends JavaAbstractOps[G] {
  this: JavaAbstract[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("abstract")
}
