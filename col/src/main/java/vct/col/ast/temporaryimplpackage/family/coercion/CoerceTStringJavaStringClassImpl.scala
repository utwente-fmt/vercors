package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceTStringJavaStringClass, JavaTClass, TJavaString, Type}

trait CoerceTStringJavaStringClassImpl[G] { this: CoerceTStringJavaStringClass[G] =>
  override def target: Type[G] = JavaTClass(ref, Seq())
}
