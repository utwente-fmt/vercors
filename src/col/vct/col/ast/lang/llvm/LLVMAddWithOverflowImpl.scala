package vct.col.ast.lang.llvm

import vct.col.ast.LLVMAddWithOverflow
import vct.col.ast.ops.LLVMAddWithOverflowOps
import vct.col.print._

trait LLVMAddWithOverflowImpl[G] extends LLVMAddWithOverflowOps[G] {
  this: LLVMAddWithOverflow[G] =>
  override def instRepr: Doc =
    if (signed) { Text("sadd.with.overflow") }
    else { Text("uadd.with.overflow") }

}
