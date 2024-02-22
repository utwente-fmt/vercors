package vct.col.ast.lang.llvm

import vct.col.ast.{LLVMTruncate, Type}
import vct.col.ast.ops.LLVMTruncateOps
import vct.col.print._

trait LLVMTruncateImpl[G] extends LLVMTruncateOps[G] {
  this: LLVMTruncate[G] =>
  override def t: Type[G] = outputType
  // override def layout(implicit ctx: Ctx): Doc = ???
}
