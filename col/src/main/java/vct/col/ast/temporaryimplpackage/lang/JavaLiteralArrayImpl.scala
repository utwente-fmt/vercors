package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{JavaLiteralArray, Type}

trait JavaLiteralArrayImpl { this: JavaLiteralArray =>
  override def t: Type = typeContext.get
}