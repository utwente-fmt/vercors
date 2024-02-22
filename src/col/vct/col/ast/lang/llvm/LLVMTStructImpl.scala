package vct.col.ast.lang.llvm

import vct.col.ast.LLVMTStruct
import vct.col.ast.ops.LLVMTStructOps
import vct.col.print._

trait LLVMTStructImpl[G] extends LLVMTStructOps[G] {
  this: LLVMTStruct[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
