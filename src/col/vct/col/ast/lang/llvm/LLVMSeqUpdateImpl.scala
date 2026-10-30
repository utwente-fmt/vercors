package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMSeqUpdateOps
import vct.col.ast.{LLVMSeqUpdate, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence}

trait LLVMSeqUpdateImpl[G] extends LLVMSeqUpdateOps[G] {
  this: LLVMSeqUpdate[G] =>
  override def t: Type[G] = elem.t

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(seq) <> ".update(" <> Doc.args(Seq(idx, elem)) <> ")")
}
