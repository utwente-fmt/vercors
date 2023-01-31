package vct.col.ast.family.coercion

import vct.col.ast.{CoerceNullStringClass, TStringClass, Type}

trait CoerceNullStringClassImpl[G] { this: CoerceNullStringClass[G] =>
  val target: Type[G] = TStringClass()
}
