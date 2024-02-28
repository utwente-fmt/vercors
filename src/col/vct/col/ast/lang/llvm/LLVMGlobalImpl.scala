package vct.col.ast.lang.llvm

import vct.col.ast.LLVMGlobal
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.LLVMGlobalOps

trait LLVMGlobalImpl[G] extends LLVMGlobalOps[G] { this: LLVMGlobal[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text(value)

}
