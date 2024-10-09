package vct.col.ast.lang.llvm

import vct.col.ast.LLVMTMetadata
import vct.col.ast.ops.LLVMTMetadataOps
import vct.col.print.{Ctx, Doc, Group, Text}

trait LLVMTMetadataImpl[G] extends LLVMTMetadataOps[G] {
  this: LLVMTMetadata[G] =>

  override def layout(implicit ctx: Ctx): Doc = Text("metadata")
}
