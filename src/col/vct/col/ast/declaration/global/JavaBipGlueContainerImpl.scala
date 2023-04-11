package vct.col.ast.declaration.global

import vct.col.ast.JavaBipGlueContainer
import vct.col.print._

trait JavaBipGlueContainerImpl[G] { this: JavaBipGlueContainer[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    job.show
}
