package vct.col.ast.temporaryimplpackage.expr.misc

import vct.col.ast.{JavaLangString, JavaStringConcat, TPinnedDecl, Type}

trait JavaStringConcatImpl[G] { this: JavaStringConcat[G] =>
  override def t: Type[G] = TPinnedDecl(JavaLangString())
}
