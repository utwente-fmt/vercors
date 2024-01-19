package vct.col.ast.lang.llvm

import vct.col.ast.LlvmGlobal
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.LlvmGlobalOps

trait LlvmGlobalImpl[G] extends LlvmGlobalOps[G] { this: LlvmGlobal[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text(value)

}
