package vct.col.ast.lang.llvm

import vct.col.ast.LLVMTPointer
import vct.col.ast.ops.LLVMTPointerOps
import vct.col.print._

trait LLVMTPointerImpl[G] extends LLVMTPointerOps[G] {
  this: LLVMTPointer[G] =>

  override def layout(implicit ctx: Ctx): Doc =
    if (innerType.isDefined)
      Group(Text("ptr") <+> innerType.get)
    else
      Text("ptr")
}
