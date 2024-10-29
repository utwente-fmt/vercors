package vct.col.ast.lang.llvm

import vct.col.ast.LLVMMemoryRelease
import vct.col.ast.ops.LLVMMemoryReleaseOps
import vct.col.print._

trait LLVMMemoryReleaseImpl[G] extends LLVMMemoryReleaseOps[G] {
  this: LLVMMemoryRelease[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("release")
}
