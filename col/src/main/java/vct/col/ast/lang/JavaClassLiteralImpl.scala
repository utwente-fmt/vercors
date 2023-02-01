package vct.col.ast.lang

import vct.col.ast.{JavaClassLiteral, Type}

// JavaBIP code needs an AST node for this concept
trait JavaClassLiteralImpl[G] { this: JavaClassLiteral[G] =>
  def t: Type[G] = ???
}
