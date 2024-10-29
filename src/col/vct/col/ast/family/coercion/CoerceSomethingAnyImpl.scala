package vct.col.ast.family.coercion

import vct.col.ast.{CoerceSomethingAny, Type, TAny}
import vct.col.ast.ops.CoerceSomethingAnyOps

trait CoerceSomethingAnyImpl[G] extends CoerceSomethingAnyOps[G] {
  this: CoerceSomethingAny[G] =>
  override def target: Type[G] = TAny()
}
