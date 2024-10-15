package vct.col.ast.lang.llvm

import vct.col.ast.{LLVMSignExtend, Type}
import vct.col.ast.ops.LLVMSignExtendOps
import vct.col.print._

trait LLVMSignExtendImpl[G] extends LLVMSignExtendOps[G] {
  this: LLVMSignExtend[G] =>
  override def t: Type[G] = outputType
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sext") <+> inputType <+> value <+> "to" <+> outputType)
}
