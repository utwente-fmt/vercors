package vct.col.ast.temporaryimplpackage.family.coercion

import vct.col.ast.{CoerceSomethingAny, TAny, Type}

trait SomethingAnyImpl[G] { this: CoerceSomethingAny[G] =>
  override def target: Type[G] = TAny()
}
