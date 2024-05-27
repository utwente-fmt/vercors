package vct.col.ast.unsorted

import vct.col.ast.JavaBipAnnotation
import vct.col.ast.ops.JavaBipAnnotationOps
import vct.col.print._

trait JavaBipAnnotationImpl[G] extends JavaBipAnnotationOps[G] {
  this: JavaBipAnnotation[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
