package vct.col.ast.declaration.global

import vct.col.ast.JavaBipGlueContainer
import vct.col.print._
import vct.col.ast.ops.JavaBipGlueContainerOps

trait JavaBipGlueContainerImpl[G] extends JavaBipGlueContainerOps[G] { this: JavaBipGlueContainer[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    job.show
}
