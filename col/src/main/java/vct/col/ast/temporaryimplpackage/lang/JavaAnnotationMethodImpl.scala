package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{Declaration, JavaAnnotationMethod}
import vct.col.ast.temporaryimplpackage.util.Declarator

trait JavaAnnotationMethodImpl[G] extends Declarator[G] { this: JavaAnnotationMethod[G] =>
  override def declarations: Seq[Declaration[G]] = Seq()
}
