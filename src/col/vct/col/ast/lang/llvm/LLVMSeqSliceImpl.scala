package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMSeqSliceOps
import vct.col.ast.{LLVMSeqSlice, LLVMTPointer, Type}
import vct.col.print.{Ctx, Doc, Precedence}

trait LLVMSeqSliceImpl[G] extends LLVMSeqSliceOps[G] {
  this: LLVMSeqSlice[G] =>
  override def t: Type[G] = seq.t.asInstanceOf[LLVMTPointer[G]].innerType.get

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    assoc(seq) <> "[" <> sIdx <> ".." <> eIdx <> "]"
}
