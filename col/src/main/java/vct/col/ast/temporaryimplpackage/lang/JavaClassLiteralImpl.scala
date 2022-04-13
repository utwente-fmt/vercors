package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{JavaClassLiteral, JavaLangClass, TPinnedDecl}

trait JavaClassLiteralImpl[G] { this: JavaClassLiteral[G] =>
  def t(): TPinnedDecl[G] = TPinnedDecl(JavaLangClass(), Seq(cls))
}
