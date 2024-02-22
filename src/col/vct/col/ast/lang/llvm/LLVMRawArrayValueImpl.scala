package vct.col.ast.lang.llvm

import vct.col.ast.{Type, LLVMRawArrayValue}
import vct.col.ast.ops.LLVMRawArrayValueOps
import vct.col.print._

trait LLVMRawArrayValueImpl[G] extends LLVMRawArrayValueOps[G] {
  this: LLVMRawArrayValue[G] =>
  override def t: Type[G] = arrayType
  // override def layout(implicit ctx: Ctx): Doc = ???
}
