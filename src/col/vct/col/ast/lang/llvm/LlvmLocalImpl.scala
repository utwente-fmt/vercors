package vct.col.ast.lang.llvm

import vct.col.ast.{LlvmLocal, Type}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.LlvmLocalOps

trait LlvmLocalImpl[G] extends LlvmLocalOps[G] {
  this: LlvmLocal[G] =>
  override lazy val t: Type[G] = ref.get.decl.t

  override def layout(implicit ctx: Ctx): Doc = Text(name)
}
