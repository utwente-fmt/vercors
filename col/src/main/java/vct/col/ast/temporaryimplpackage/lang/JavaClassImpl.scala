package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{JavaClass, PinnedDecl, Type}

trait JavaClassImpl[G] { this: JavaClass[G] =>
  override def supports: Seq[Type[G]] = ext +: imp
}