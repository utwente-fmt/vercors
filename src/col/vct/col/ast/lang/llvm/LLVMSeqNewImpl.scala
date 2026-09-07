package vct.col.ast.lang.llvm

import vct.col.ast.ops.{LLVMPtrLengthOps, LLVMSeqNewOps}
import vct.col.ast.{LLVMPtrLength, LLVMSeqNew, LLVMTInt, TSeq, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait LLVMSeqNewImpl[G] extends LLVMSeqNewOps[G] {
  this: LLVMSeqNew[G] =>

  override def layout(implicit ctx: Ctx): Doc =
    Group(target.show <+> "=" <+> Group(Text("seq<") <> cType <> ">{ }"))
}
