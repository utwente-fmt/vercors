package vct.col.ast.family.coercion

import vct.col.ast.{CoerceCArrayPointer, TPointer}
import vct.col.ast.ops.CoerceCArrayPointerOps

trait CoerceCArrayPointerImpl[G] extends CoerceCArrayPointerOps[G] {
  this: CoerceCArrayPointer[G] =>
  override def target: TPointer[G] = TPointer(elementType)
}
