package vct.col.ast.lang.llvm

import vct.col.ast.LLVMArgAttribute
import vct.col.ast.ops.LLVMArgAttributeFamilyOps

trait LLVMArgAttributeImpl[G] extends LLVMArgAttributeFamilyOps[G] {
  this: LLVMArgAttribute[G] =>

}
