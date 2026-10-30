package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMSeqPrependOps
import vct.col.ast.{LLVMSeqPrepend, LLVMTPointer, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait LLVMSeqPrependImpl[G] extends LLVMSeqPrependOps[G] {
  this: LLVMSeqPrepend[G] =>
  override def t: Type[G] = seq.t.asInstanceOf[LLVMTPointer[G]].innerType.get

  override def precedence: Int = Precedence.SEQUENCE
  override def layout(implicit ctx: Ctx): Doc = rassoc(elem, "::", seq)
}
