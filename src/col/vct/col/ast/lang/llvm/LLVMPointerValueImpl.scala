package vct.col.ast.lang.llvm

import vct.col.ast.{
  LLVMGlobalVariable,
  LLVMPointerValue,
  LLVMTPointer,
  Type,
  HeapVariable,
}
import vct.col.ast.ops.LLVMPointerValueOps
import vct.col.print._

trait LLVMPointerValueImpl[G] extends LLVMPointerValueOps[G] {
  this: LLVMPointerValue[G] =>
  override lazy val t: Type[G] = {
    value.decl match {
      case LLVMGlobalVariable(variableType, _, _) =>
        LLVMTPointer(Some(variableType))
      case v: HeapVariable[G] => LLVMTPointer(Some(v.t))
    }
  }
  override def layout(implicit ctx: Ctx): Doc = Text("@") <> ctx.name(value)
}
