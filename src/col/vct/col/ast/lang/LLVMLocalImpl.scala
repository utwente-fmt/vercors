package vct.col.ast.lang

import vct.col.ast.{LlvmLocal, Type}
import vct.col.print.{Ctx, Doc, Text}

trait LLVMLocalImpl[G] { this: LlvmLocal[G] =>
  override lazy val t: Type[G] = ref.get.decl.t

  override def layout(implicit ctx: Ctx): Doc = Text(name)
}
