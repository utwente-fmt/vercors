package vct.col.ast.lang

import vct.col.ast.{JavaLiteralArray, Type}

trait JavaLiteralArrayImpl[G] { this: JavaLiteralArray[G] =>
  override def t: Type[G] = typeContext.get
}