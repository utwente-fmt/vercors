package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMForallOps
import vct.col.ast.{LLVMForall, TBool, Type}
import vct.col.print._

trait LLVMForallImpl[G] extends LLVMForallOps[G] {
  this: LLVMForall[G] =>

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Text("(\\forall") <+> bindingExpr.show <> ";" <>> bodyExpr.show </> ")"
    )
}
