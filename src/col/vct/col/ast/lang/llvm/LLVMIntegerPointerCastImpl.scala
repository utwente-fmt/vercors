package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMIntegerPointerCastOps
import vct.col.ast.{LLVMIntegerPointerCast, Type}
import vct.col.print._

trait LLVMIntegerPointerCastImpl[G] extends LLVMIntegerPointerCastOps[G] {
  this: LLVMIntegerPointerCast[G] =>
  override def t: Type[G] = outputType
  override def layout(implicit ctx: Ctx): Doc = {
    Group(
      if (outputType.asPointer.isDefined) { Text("inttoptr") }
      else
        { Text("ptrtoint") } <+> inputType <+> value <+> "to" <+> outputType
    )
  }
}
