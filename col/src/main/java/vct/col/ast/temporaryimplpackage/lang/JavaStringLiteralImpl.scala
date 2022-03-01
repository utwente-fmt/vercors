package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{JavaStringLiteral, Type}
import vct.col.resolve.Java

trait JavaStringLiteralImpl[G] { this: JavaStringLiteral[G] =>
  override def t: Type[G] = PinnedType(JavaLangString)// Java.JAVA_LANG_STRING
}
