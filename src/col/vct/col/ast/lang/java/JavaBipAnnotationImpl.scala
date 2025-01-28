package vct.col.ast.lang.java

import vct.col.ast.JavaBipAnnotation
import vct.col.ast.ops.JavaBipAnnotationOps

trait JavaBipAnnotationImpl[G] extends JavaBipAnnotationOps[G] {
  this: JavaBipAnnotation[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
