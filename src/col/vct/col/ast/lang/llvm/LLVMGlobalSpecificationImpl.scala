package vct.col.ast.lang.llvm

import vct.col.ast.LLVMGlobalSpecification
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.LLVMGlobalSpecificationOps

trait LLVMGlobalSpecificationImpl[G] extends LLVMGlobalSpecificationOps[G] {
  this: LLVMGlobalSpecification[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text(value)

}
