package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMFloatExtendOps
import vct.col.ast.{LLVMFloatExtend, Type}
import vct.col.print._

trait LLVMFloatExtendImpl[G] extends LLVMFloatExtendOps[G] {
  this: LLVMFloatExtend[G] =>
  override def t: Type[G] = outputType
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("fpext") <+> inputType <+> value <+> "to" <+> outputType)
}
