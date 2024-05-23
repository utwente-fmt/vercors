package vct.col.ast.family.coercion

import vct.col.ast.{CoerceTArrayAnyClass, TAnyClass}
import vct.col.ast.ops.CoerceTArrayAnyClassOps

trait CoerceTArrayAnyClassImpl[G] extends CoerceTArrayAnyClassOps[G] { this: CoerceTArrayAnyClass[G] =>
  override def target: TAnyClass[G] = TAnyClass()
}
