package vct.col.ast.family.coercion

import vct.col.ast.{CoerceCIntBool, TBool}
import vct.col.ast.ops.CoerceCIntBoolOps

trait CoerceCIntBoolImpl[G] extends CoerceCIntBoolOps[G] {
  this: CoerceCIntBool[G] =>
  override def target: TBool[G] = TBool()

}
