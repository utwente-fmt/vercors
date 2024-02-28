package vct.col.ast.unsorted

import vct.col.ast.{LLVMStore, Type}
import vct.col.ast.ops.LLVMStoreOps

trait LLVMStoreImpl[G] extends LLVMStoreOps[G] { this: LLVMStore[G] =>
  override lazy val t: Type[G] = this.store_type
}
