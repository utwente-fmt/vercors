package vct.col.ast.family.coercion

import vct.col.ast.{CoerceSomethingAnyValue, TAnyValue, Type}
import vct.col.ast.ops.CoerceSomethingAnyValueOps

trait CoerceSomethingAnyValueImpl[G] extends CoerceSomethingAnyValueOps[G] {
  this: CoerceSomethingAnyValue[G] =>
  override def target: Type[G] = TAnyValue()
}
