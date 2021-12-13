package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{SilverDeref, Type}

trait SilverDerefImpl[G] { this: SilverDeref[G] =>
  override def t: Type[G] = field.decl.t
}