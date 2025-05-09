package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMPtrBlockOffsetOps
import vct.col.ast.{LLVMPtrBlockOffset, LLVMTInt, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait LLVMPtrBlockOffsetImpl[G] extends LLVMPtrBlockOffsetOps[G] {
  this: LLVMPtrBlockOffset[G] =>
  override def t: Type[G] = LLVMTInt(64)

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Text("\\pointer_block_offset(") <> ptr.decl.show <> ")"
}
