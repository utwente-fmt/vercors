package vct.col.ast.lang

import vct.col.ast.{JavaClassLiteral, Type}

trait JavaClassLiteralImpl[G] { this: JavaClassLiteral[G] =>
  def t: Type[G] = ???
}
