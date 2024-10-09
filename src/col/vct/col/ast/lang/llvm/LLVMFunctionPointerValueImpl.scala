package vct.col.ast.lang.llvm

import vct.col.ast.{Type, LLVMTPointer, LLVMFunctionPointerValue}
import vct.col.ast.ops.LLVMFunctionPointerValueOps
import vct.col.print._

trait LLVMFunctionPointerValueImpl[G] extends LLVMFunctionPointerValueOps[G] {
  this: LLVMFunctionPointerValue[G] =>
  // TODO: Do we want a separate type for function pointers? For now we don't support function pointers anyway
  override def t: Type[G] = LLVMTPointer(None)
  override def layout(implicit ctx: Ctx): Doc = Text("@") <> ctx.name(value)
}
