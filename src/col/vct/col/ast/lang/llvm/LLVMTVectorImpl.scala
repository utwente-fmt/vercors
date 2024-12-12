package vct.col.ast.lang.llvm

import vct.col.ast.LLVMTVector
import vct.col.ast.ops.LLVMTVectorOps
import vct.col.print._

trait LLVMTVectorImpl[G] extends LLVMTVectorOps[G] {
  this: LLVMTVector[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("<") <> numElements.toString <+> "x" <+> elementType <> ">")
}
