package vct.col.ast.lang.sycl

import vct.col.ast.{CPPExprOrTypeSpecifier, Expr, SYCLTHandler, SYCLTSubGroup}
import vct.col.ast.ops.{SYCLTHandlerOps, SYCLTSubGroupOps}
import vct.col.print.{Ctx, Doc, Text}
import vct.col.resolve.ctx.{CPPInvocationTarget, RefSYCLConstructorDefinition}

trait SYCLTSubGroupImpl[G] extends SYCLTSubGroupOps[G] {
  this: SYCLTSubGroup[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("sycl::sub_group")

  override val namespacePath = "sycl::sub_group"

  def findConstructor(genericArgs: Seq[CPPExprOrTypeSpecifier[G]], args: Seq[Expr[G]]): Option[CPPInvocationTarget[G]] = {
    Some(RefSYCLConstructorDefinition(SYCLTSubGroup()))
  }



}
