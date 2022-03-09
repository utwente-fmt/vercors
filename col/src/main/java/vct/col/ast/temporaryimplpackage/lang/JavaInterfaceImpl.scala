package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{JavaInterface, SpecialDecl, Type}

trait JavaInterfaceImpl[G] { this: JavaInterface[G] =>
  override def supports: Seq[Type[G]] = ext
  override def isSpecial(s: SpecialDecl[G]): Boolean = special.contains(s)
}