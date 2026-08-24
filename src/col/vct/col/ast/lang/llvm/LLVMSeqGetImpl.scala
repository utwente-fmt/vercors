package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMSeqGetOps
import vct.col.ast.{LLVMSeqGet, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence}

trait LLVMSeqGetImpl[G] extends LLVMSeqGetOps[G] {
  this: LLVMSeqGet[G] =>
  override def t: Type[G] = elemType

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Group(assoc(seq) <> "[" <> Doc.arg(idx) <> "]")
}
