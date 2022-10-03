package vct.col.ast.lang

import vct.col.ast.{Declaration, JavaAnnotationMethod}
import vct.col.ast.util.Declarator

trait JavaAnnotationMethodImpl[G] extends Declarator[G] { this: JavaAnnotationMethod[G] =>
  override def declarations: Seq[Declaration[G]] = Seq()
  override def isStatic = false
}
