package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMSeqUpdateOps
import vct.col.ast.{LLVMSeqUpdate, LLVMTPointer, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence}

trait LLVMSeqUpdateImpl[G] extends LLVMSeqUpdateOps[G] {
  this: LLVMSeqUpdate[G] =>
  override def t: Type[G] = seq.t.asInstanceOf[LLVMTPointer[G]].innerType.get

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(seq) <> ".update(" <> Doc.args(Seq(idx, elem)) <> ")")
}
