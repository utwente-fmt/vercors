package vct.col.ast.family.coercion

import vct.col.ast.ops.CoercePointerBoolOps
import vct.col.ast.{CoercePointerBool, TBool, Type}

trait CoercePointerBoolImpl[G] extends CoercePointerBoolOps[G] {
  this: CoercePointerBool[G] =>
  override def target: Type[G] = TBool()(o)
  // override def layout(implicit ctx: Ctx): Doc = ???
}
