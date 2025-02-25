package vct.col.ast.lang.java

import vct.col.ast.{Declaration, JavaAnnotationMethod}
import vct.col.ast.util.Declarator
import vct.col.ast.ops.JavaAnnotationMethodOps

trait JavaAnnotationMethodImpl[G]
    extends Declarator[G] with JavaAnnotationMethodOps[G] {
  this: JavaAnnotationMethod[G] =>
  override def declarations: Seq[Declaration[G]] = Seq()
  override def isStatic = false
}
