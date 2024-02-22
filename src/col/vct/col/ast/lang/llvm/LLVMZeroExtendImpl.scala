package vct.col.ast.lang.llvm

import vct.col.ast.{LLVMZeroExtend, Type}
import vct.col.ast.ops.LLVMZeroExtendOps
import vct.col.print._

trait LLVMZeroExtendImpl[G] extends LLVMZeroExtendOps[G] {
  this: LLVMZeroExtend[G] =>
  override def t: Type[G] = outputType
  // override def layout(implicit ctx: Ctx): Doc = ???
}
