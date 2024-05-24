package vct.col.ast.family.coercion

import vct.col.ast.ops.CoercePointerBoolOps
import vct.col.ast.{CoercePointerBool, TBool}

trait CoercePointerBoolImpl[G] extends CoercePointerBoolOps[G] {
  this: CoercePointerBool[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
  override def target: TBool[G] = TBool()
}
