package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMBoundVarOps
import vct.col.ast.{LLVMBoundVar, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}

trait LLVMBoundVarImpl[G] extends LLVMBoundVarOps[G] {
  this: LLVMBoundVar[G] =>
  override def t: Type[G] = varType

  override def precedence: Int = Precedence.ATOMIC

  override def layout(implicit ctx: Ctx): Doc =
    Text("BV<") <> varType.show <> Text(">(") <> id <> Text(")")
}
