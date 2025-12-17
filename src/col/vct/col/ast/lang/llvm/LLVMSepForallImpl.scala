package vct.col.ast.lang.llvm

import vct.col.ast.ops.LLVMSepForallOps
import vct.col.ast.{LLVMSepForall, TBool, Type}
import vct.col.print._

trait LLVMSepForallImpl[G] extends LLVMSepForallOps[G] {
  this: LLVMSepForall[G] =>

  override def precedence: Int = Precedence.ATOMIC

  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Text("(\\forall*") <+> bindingExpr.show <> ";" <>> bodyExpr.show </> ")"
    )
}
