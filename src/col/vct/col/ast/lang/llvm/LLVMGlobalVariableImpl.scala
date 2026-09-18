package vct.col.ast.lang.llvm

import vct.col.ast.LLVMGlobalVariable
import vct.col.ast.ops.LLVMGlobalVariableOps
import vct.col.print._

trait LLVMGlobalVariableImpl[G] extends LLVMGlobalVariableOps[G] {
  this: LLVMGlobalVariable[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text("@") <> ctx.name(this) <+> "=" <+>
      (if (constant)
         "constant"
       else
         "global") <+>
      (if (value.isDefined) { variableType.show <+> value.get }
       else { variableType })
}
