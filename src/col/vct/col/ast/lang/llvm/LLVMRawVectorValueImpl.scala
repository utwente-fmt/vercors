package vct.col.ast.lang.llvm

import vct.col.ast.{Type, LLVMRawVectorValue}
import vct.col.ast.ops.LLVMRawVectorValueOps
import vct.col.print._

trait LLVMRawVectorValueImpl[G] extends LLVMRawVectorValueOps[G] {
  this: LLVMRawVectorValue[G] =>
  override def t: Type[G] = vectorType
  // override def layout(implicit ctx: Ctx): Doc = ???
}
