package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMPtrBlockLengthOps
import vct.col.ast.{LLVMPtrBlockLength, LLVMTInt, Type}
import vct.col.print._

trait LLVMPtrBlockLengthImpl[G] extends LLVMPtrBlockLengthOps[G] {
  this: LLVMPtrBlockLength[G] =>
  override def t: Type[G] = LLVMTInt(64)

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Text("\\pointer_block_length(") <> ptr.decl.show <> ")"
}
