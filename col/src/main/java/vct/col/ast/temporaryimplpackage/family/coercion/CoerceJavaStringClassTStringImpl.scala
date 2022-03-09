package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceJavaStringClassTString, TJavaString, Type}

trait CoerceJavaStringClassTStringImpl[G] { this: CoerceJavaStringClassTString[G] =>
  override def target: Type[G] = TJavaString()
}
