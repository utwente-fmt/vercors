package vct.col.ast.family.coercion

import vct.col.ast.ops.CoerceLLVMArrayOps
import vct.col.ast.CoerceLLVMArray

trait CoerceLLVMArrayImpl[G] extends CoerceLLVMArrayOps[G] {
  this: CoerceLLVMArray[G] =>
}
