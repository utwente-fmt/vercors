package vct.col.ast.lang.llvm

import vct.col.ast.LLVMFunctionType
import vct.col.ast.ops.LLVMFunctionTypeFamilyOps

trait LLVMFunctionTypeImpl[G] extends LLVMFunctionTypeFamilyOps[G] {
  this: LLVMFunctionType[G] =>

}
