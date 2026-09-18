package vct.col.ast.family.coercion

import vct.col.ast.ops.CoerceNullLLVMPointerOps
import vct.col.ast.{CoerceNullLLVMPointer, LLVMTPointer}

trait CoerceNullLLVMPointerImpl[G] extends CoerceNullLLVMPointerOps[G] {
  this: CoerceNullLLVMPointer[G] =>
  override def target: LLVMTPointer[G] = LLVMTPointer(elementType)
}
