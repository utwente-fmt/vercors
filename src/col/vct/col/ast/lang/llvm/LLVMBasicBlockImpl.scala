package vct.col.ast.lang.llvm

import vct.col.ast.LLVMBasicBlock
import vct.col.ast.ops.LLVMBasicBlockOps
import vct.col.print._

trait LLVMBasicBlockImpl[G] extends LLVMBasicBlockOps[G] {
  this: LLVMBasicBlock[G] =>

  override def layout(implicit ctx: Ctx): Doc =
    label.show <> ":" <+> body.layoutAsBlock
}
