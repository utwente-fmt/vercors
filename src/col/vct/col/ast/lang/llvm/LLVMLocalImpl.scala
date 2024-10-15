package vct.col.ast.lang.llvm

import vct.col.ast.{LLVMLocal, Type}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.ast.ops.LLVMLocalOps

trait LLVMLocalImpl[G] extends LLVMLocalOps[G] {
  this: LLVMLocal[G] =>
  override lazy val t: Type[G] = ref.get.decl.t

  override def layout(implicit ctx: Ctx): Doc = Text(name)
}
