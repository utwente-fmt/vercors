package vct.col.ast.family.coercion

import vct.col.ast.{CoerceLLVMIntInt, TInt}
import vct.col.ast.ops.CoerceLLVMIntIntOps

trait CoerceLLVMIntIntImpl[G] extends CoerceLLVMIntIntOps[G] {
  this: CoerceLLVMIntInt[G] =>
  override def target: TInt[G] = TInt()
}
