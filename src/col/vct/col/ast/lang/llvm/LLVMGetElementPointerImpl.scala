package vct.col.ast.lang.llvm

import vct.col.ast.{LLVMGetElementPointer, LLVMTPointer, Type}
import vct.col.ast.ops.LLVMGetElementPointerOps
import vct.col.print._

trait LLVMGetElementPointerImpl[G] extends LLVMGetElementPointerOps[G] {
  this: LLVMGetElementPointer[G] =>
  override def t: Type[G] = LLVMTPointer(Some(resultType))
  // override def layout(implicit ctx: Ctx): Doc = ???
}
