package vct.col.ast.lang.llvm

import vct.col.ast.LLVMTMetadata
import vct.col.ast.ops.LLVMTMetadataOps

trait LLVMTMetadataImpl[G] extends LLVMTMetadataOps[G] {
  this: LLVMTMetadata[G] =>

}
