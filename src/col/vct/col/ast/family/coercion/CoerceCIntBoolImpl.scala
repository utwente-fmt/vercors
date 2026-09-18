package vct.col.ast.family.coercion

import vct.col.ast.{CoerceCIntBool, TBool, Type}
import vct.col.ast.ops.CoerceCIntBoolOps

trait CoerceCIntBoolImpl[G] extends CoerceCIntBoolOps[G] {
  this: CoerceCIntBool[G] =>
  override def target: Type[G] = TBool()(o)
  // override def layout(implicit ctx: Ctx): Doc = ???
}
