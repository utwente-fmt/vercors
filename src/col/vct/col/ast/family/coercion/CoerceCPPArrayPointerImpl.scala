package vct.col.ast.family.coercion

import vct.col.ast.{CoerceCPPArrayPointer, TPointer}
import vct.col.ast.ops.CoerceCPPArrayPointerOps

trait CoerceCPPArrayPointerImpl[G] extends CoerceCPPArrayPointerOps[G] {
  this: CoerceCPPArrayPointer[G] =>
  override def target: TPointer[G] = TPointer(elementType)
}
