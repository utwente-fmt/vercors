package vct.col.ast.unsorted

import vct.col.ast.{Constructor, Statement, TClass}
import vct.col.ast.ops.ConstructorOps
import vct.col.print._

trait ConstructorImpl[G] extends ConstructorOps[G] { this: Constructor[G] =>
  override def pure: Boolean = false
  override def returnType: TClass[G] = TClass(cls)

  // override def layout(implicit ctx: Ctx): Doc = ???
}
