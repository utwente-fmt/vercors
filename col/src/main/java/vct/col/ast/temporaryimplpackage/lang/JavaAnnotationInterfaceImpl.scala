package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{JavaAnnotationInterface, Type, Variable}

trait JavaAnnotationInterfaceImpl[G] { this: JavaAnnotationInterface[G] =>
  override def supports: Seq[Type[G]] = Seq(ext)
  override def typeParams: Seq[Variable[G]] = Seq()
}