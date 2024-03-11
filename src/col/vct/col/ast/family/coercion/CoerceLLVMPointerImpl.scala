package vct.col.ast.family.coercion

import vct.col.ast.{CoerceLLVMPointer, LLVMTPointer}
import vct.col.ast.ops.CoerceLLVMPointerOps

trait CoerceLLVMPointerImpl[G] extends CoerceLLVMPointerOps[G] { this: CoerceLLVMPointer[G] =>
  override def target: LLVMTPointer[G] = LLVMTPointer(to)
}
