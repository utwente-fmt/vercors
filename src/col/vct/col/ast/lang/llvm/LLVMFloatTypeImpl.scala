package vct.col.ast.lang.llvm

import vct.col.ast.LLVMFloatType
import vct.col.ast.ops.LLVMFloatTypeFamilyOps

trait LLVMFloatTypeImpl[G] extends LLVMFloatTypeFamilyOps[G] {
  this: LLVMFloatType[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???

}
