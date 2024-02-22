package vct.col.ast.lang.llvm

import vct.col.ast.{LLVMStructValue, Type}
import vct.col.ast.ops.LLVMStructValueOps
import vct.col.print._

trait LLVMStructValueImpl[G] extends LLVMStructValueOps[G] {
  this: LLVMStructValue[G] =>
  override def t: Type[G] = structType
  // override def layout(implicit ctx: Ctx): Doc = ???
}
