package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{CStructDeref, Type}

trait CStructDerefImpl[G] { this: CStructDeref[G] =>
  override def t: Type[G] = ???
}