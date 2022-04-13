package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{JavaLangString, JavaStringLiteral, TPinnedDecl}

trait JavaStringLiteralImpl[G] { this: JavaStringLiteral[G] =>
  def t: TPinnedDecl[G] = TPinnedDecl[G](JavaLangString(), Nil)
}
