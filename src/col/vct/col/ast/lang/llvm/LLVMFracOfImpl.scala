package vct.col.ast.lang.llvm

import vct.col.ast.LLVMFracOf
import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.ops.LLVMFracOfOps

trait LLVMFracOfImpl[G] extends NodeFamilyImpl[G] with LLVMFracOfOps[G] {
  this: LLVMFracOf[G] =>

}
