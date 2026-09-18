package vct.col.ast.lang.llvm

import vct.col.ast.LLVMTFunction
import vct.col.ast.ops.LLVMTFunctionOps
import vct.col.print._

trait LLVMTFunctionImpl[G] extends LLVMTFunctionOps[G] {
  this: LLVMTFunction[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
