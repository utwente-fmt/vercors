package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMMultWithOverflowOps
import vct.col.ast.{LLVMMultWithOverflow, Type}
import vct.col.print._

trait LLVMMultWithOverflowImpl[G] extends LLVMMultWithOverflowOps[G] {
  this: LLVMMultWithOverflow[G] =>
  override def instRepr: Doc =
    if (signed) { Text("smul.with.overflow") }
    else { Text("umul.with.overflow") }
}
