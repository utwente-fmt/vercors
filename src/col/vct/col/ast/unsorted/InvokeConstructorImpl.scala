package vct.col.ast.unsorted

import vct.col.ast.InvokeConstructor
import vct.col.ast.ops.InvokeConstructorOps
import vct.col.print._

trait InvokeConstructorImpl[G] extends InvokeConstructorOps[G] { this: InvokeConstructor[G] =>
  override def outArgs: Seq[Nothing] = Nil
  // override def layout(implicit ctx: Ctx): Doc = ???
}
