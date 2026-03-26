package vct.col.ast.lang.llvm

import vct.col.ast.ops.{LLVMSeqEqOps, LLVMSeqSizeOps}
import vct.col.ast.{LLVMSeqEq, LLVMSeqSize, TBool, TInt, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait LLVMSeqEqImpl[G] extends LLVMSeqEqOps[G] {
  this: LLVMSeqEq[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.EQUALITY
  override def layout(implicit ctx: Ctx): Doc = lassoc(s1, "==", s2)
}
