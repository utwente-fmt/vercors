package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{JavaNewClass, Type}

trait JavaNewClassImpl { this: JavaNewClass =>
  override def t: Type = name
}