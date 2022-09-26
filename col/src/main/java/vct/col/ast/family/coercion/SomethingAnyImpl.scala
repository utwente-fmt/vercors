package vct.col.ast.family.coercion

import vct.col.ast.{CoerceSomethingAny, TAny, Type}

trait SomethingAnyImpl[G] { this: CoerceSomethingAny[G] =>
  override def target: Type[G] = TAny()
}
