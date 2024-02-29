package vct.col.ast.unsorted

import vct.col.ast.LLVMTArray
import vct.col.ast.ops.LLVMTArrayOps
import vct.col.print._

trait LLVMTArrayImpl[G] extends LLVMTArrayOps[G] { this: LLVMTArray[G] =>
  // override def layout(implicit ctx: Ctx): Doc = ???
}
