package vct.col.ast.family.coercion

import vct.col.ast.{CoerceBoolCInt, TBool}
import vct.col.ast.ops.CoerceBoolCIntOps

trait CoerceBoolCIntImpl[G] extends CoerceBoolCIntOps[G] {
  this: CoerceBoolCInt[G] =>
  override def target: TBool[G] = TBool()

}
