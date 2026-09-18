package vct.col.ast.unsorted

import vct.col.ast.LLVMPredicateApply
import vct.col.ast.ops.LLVMPredicateApplyOps
import vct.col.print._

trait LLVMPredicateApplyImpl[G] extends LLVMPredicateApplyOps[G] {
  this: LLVMPredicateApply[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
