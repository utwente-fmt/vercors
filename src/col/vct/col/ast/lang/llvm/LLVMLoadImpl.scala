package vct.col.ast.lang.llvm

import vct.col.ast.{LLVMLoad, Type}
import vct.col.ast.ops.LLVMLoadOps

trait LLVMLoadImpl[G] extends LLVMLoadOps[G] {
  this: LLVMLoad[G] =>
}
