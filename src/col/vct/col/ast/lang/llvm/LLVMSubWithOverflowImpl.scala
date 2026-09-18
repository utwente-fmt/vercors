package vct.col.ast.lang.llvm

import vct.col.ast.LLVMSubWithOverflow
import vct.col.ast.ops.LLVMSubWithOverflowOps
import vct.col.print._

trait LLVMSubWithOverflowImpl[G] extends LLVMSubWithOverflowOps[G] {
  this: LLVMSubWithOverflow[G] =>
  override def instRepr: Doc =
    if (signed) { Text("ssub.with.overflow") }
    else { Text("usub.with.overflow") }
}
