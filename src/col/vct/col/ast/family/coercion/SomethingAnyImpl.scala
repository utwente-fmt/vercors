package vct.col.ast.family.coercion

import vct.col.ast.{CoerceSomethingAny, Type, TAny}

trait SomethingAnyImpl[G] { this: CoerceSomethingAny[G] =>
  override def target: Type[G] = TAny()
}
