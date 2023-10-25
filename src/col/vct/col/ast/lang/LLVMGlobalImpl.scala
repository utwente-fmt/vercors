package vct.col.ast.lang

import vct.col.ast.LlvmGlobal
import vct.col.print.{Ctx, Doc, Text}

trait LLVMGlobalImpl[G] { this: LlvmGlobal[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text(value)

}
