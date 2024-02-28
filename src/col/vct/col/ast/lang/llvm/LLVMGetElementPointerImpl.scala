package vct.col.ast.unsorted

import vct.col.ast.{LLVMGetElementPointer, Type}
import vct.col.ast.ops.LLVMGetElementPointerOps
import vct.col.print._

trait LLVMGetElementPointerImpl[G] extends LLVMGetElementPointerOps[G] { this: LLVMGetElementPointer[G] =>
  override def t: Type[G] = result_type
  // override def layout(implicit ctx: Ctx): Doc = ???
}
