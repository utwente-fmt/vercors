package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{JavaNewClass, Type}

trait JavaNewClassImpl[G] { this: JavaNewClass[G] =>
  override def t: Type[G] = name
}