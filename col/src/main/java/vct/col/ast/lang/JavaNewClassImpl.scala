package vct.col.ast.lang

import vct.col.ast.{JavaNewClass, Type}

trait JavaNewClassImpl[G] { this: JavaNewClass[G] =>
  override def t: Type[G] = name
}