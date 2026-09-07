package vct.col.ast.family.coercion

import vct.col.ast.ops.CoerceLLVMIntBoolOps
import vct.col.ast.{CoerceLLVMIntBool, TBool}

trait CoerceLLVMIntBoolImpl[G] extends CoerceLLVMIntBoolOps[G] {
  this: CoerceLLVMIntBool[G] =>
  override def target: TBool[G] = TBool()
}
