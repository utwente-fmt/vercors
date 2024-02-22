package vct.col.ast.lang.llvm

import vct.col.ast.{Type, LLVMVectorValue}
import vct.col.print._
import vct.col.ast.ops.LLVMVectorValueOps

trait LLVMVectorValueImpl[G] extends LLVMVectorValueOps[G] {
  this: LLVMVectorValue[G] =>
  override def t: Type[G] = vectorType
  // override def layout(implicit ctx: Ctx): Doc = ???
}
