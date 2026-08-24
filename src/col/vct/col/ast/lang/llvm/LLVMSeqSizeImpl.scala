package vct.col.ast.lang.llvm

import vct.col.ast.ops.{LLVMSeqSizeOps, SizeOps}
import vct.col.ast.{LLVMSeqSize, Size, TInt, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait LLVMSeqSizeImpl[G] extends LLVMSeqSizeOps[G] {
  this: LLVMSeqSize[G] =>
  override def t: Type[G] = TInt()

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("|") <> seq <> "|"
}
