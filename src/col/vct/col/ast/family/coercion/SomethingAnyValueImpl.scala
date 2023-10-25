package vct.col.ast.family.coercion

import vct.col.ast.{CoerceSomethingAnyValue, TAnyValue, Type}

trait SomethingAnyValueImpl[G] { this: CoerceSomethingAnyValue[G] =>
  override def target: Type[G] = TAnyValue()
}
