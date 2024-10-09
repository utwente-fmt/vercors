package vct.col.ast.lang.llvm

import vct.col.ast.{LLVMMemoryNotAtomic, LLVMStore}
import vct.col.ast.ops.LLVMStoreOps
import vct.col.print._

trait LLVMStoreImpl[G] extends LLVMStoreOps[G] {
  this: LLVMStore[G] =>

  private def layoutOrdering(inner: Doc)(implicit ctx: Ctx): Doc =
    if (ordering.isInstanceOf[LLVMMemoryNotAtomic[_]]) { Group(inner) }
    else { Group(inner <+> ordering) }

  override def layout(implicit ctx: Ctx): Doc =
    layoutOrdering(Text("store") <+> value <> "," <+> pointer)
}
