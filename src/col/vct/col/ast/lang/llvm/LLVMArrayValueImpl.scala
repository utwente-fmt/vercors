package vct.col.ast.lang.llvm

import vct.col.ast.{Type, LLVMArrayValue}
import vct.col.ast.ops.LLVMArrayValueOps
import vct.col.print._

trait LLVMArrayValueImpl[G] extends LLVMArrayValueOps[G] {
  this: LLVMArrayValue[G] =>
  override def t: Type[G] = arrayType
  // override def layout(implicit ctx: Ctx): Doc = ???
}
