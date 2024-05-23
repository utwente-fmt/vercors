package vct.col.ast.lang.java

import vct.col.ast.{JavaAnnotationInterface, Type, Variable}
import vct.col.ast.ops.JavaAnnotationInterfaceOps

trait JavaAnnotationInterfaceImpl[G] extends JavaAnnotationInterfaceOps[G] { this: JavaAnnotationInterface[G] =>
  override def supports: Seq[Type[G]] = Seq(ext)
  override def typeParams: Seq[Variable[G]] = Seq()
}