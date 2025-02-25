package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMPtrLengthOps
import vct.col.ast.{LLVMPtrLength, LLVMTInt, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait LLVMPtrLengthImpl[G] extends LLVMPtrLengthOps[G] {
  this: LLVMPtrLength[G] =>
  override def t: Type[G] = LLVMTInt(64)

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Text("\\pointer_length(") <> ptr.decl.show <> ")"
}
