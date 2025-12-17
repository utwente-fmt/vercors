package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMExistsOps
import vct.col.ast.{LLVMExists, TBool, Type}
import vct.col.print._

trait LLVMExistsImpl[G] extends LLVMExistsOps[G] {
  this: LLVMExists[G] =>

  override def precedence: Int = Precedence.ATOMIC

  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Text("(\\exists") <+> bindingExpr.show <> ";" <>> bodyExpr.show </> ")"
    )
}
