package vct.col.ast.unsorted

import vct.col.ast.{Type, LLVMPointerValue, LLVMTPointer}
import vct.col.ast.ops.LLVMPointerValueOps
import vct.col.print._

trait LLVMPointerValueImpl[G] extends LLVMPointerValueOps[G] { this: LLVMPointerValue[G] =>
  override def t: Type[G] = LLVMTPointer()
  // override def layout(implicit ctx: Ctx): Doc = ???
}
