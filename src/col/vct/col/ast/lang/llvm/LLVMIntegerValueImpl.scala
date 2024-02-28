package vct.col.ast.unsorted

import vct.col.ast.{Type, LLVMIntegerValue}
import vct.col.ast.ops.LLVMIntegerValueOps
import vct.col.print._

trait LLVMIntegerValueImpl[G] extends LLVMIntegerValueOps[G] { this: LLVMIntegerValue[G] =>
  override def t: Type[G] = integer_type
  // override def layout(implicit ctx: Ctx): Doc = ???
}
