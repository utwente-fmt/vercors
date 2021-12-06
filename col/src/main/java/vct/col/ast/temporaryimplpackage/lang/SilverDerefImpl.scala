package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{SilverDeref, Type}

trait SilverDerefImpl { this: SilverDeref =>
  override def t: Type = field.decl.t
}