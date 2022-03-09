package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{JavaClass, SpecialDecl, Type}

trait JavaClassImpl[G] { this: JavaClass[G] =>
  override def supports: Seq[Type[G]] = ext +: imp
  override def isSpecial(s: SpecialDecl[G]): Boolean = special.contains(s)
}