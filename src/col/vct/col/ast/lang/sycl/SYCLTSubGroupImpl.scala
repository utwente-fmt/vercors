package vct.col.ast.lang.sycl

import vct.col.ast.{ApplicableContract, ArraySubscript, CPPExprOrTypeSpecifier, CPPInvocation, Eq, Expr, Forall, GreaterEq, Local, Old, PermPointer, PointerSubscript, Procedure, Result, SYCLTHandler, SYCLTSubGroup, Star, TArray, TCInt, UnitAccountedPredicate, WritePerm}
import vct.col.ast.ops.{SYCLTHandlerOps, SYCLTSubGroupOps}
import vct.col.origin.PanicBlame
import vct.col.print.{Ctx, Doc, Text}
import vct.col.resolve.ctx.{CPPInvocationTarget, RefSYCLConstructorDefinition}
import vct.col.util.AstBuildHelpers.{c_const, foldStar, tt, withResult}

trait SYCLTSubGroupImpl[G] extends SYCLTSubGroupOps[G] {
  this: SYCLTSubGroup[G] =>
  override def layout(implicit ctx: Ctx): Doc = Text("sycl::sub_group")

  override val namespacePath = "sycl::sub_group"

  def findConstructor(genericArgs: Seq[CPPExprOrTypeSpecifier[G]], args: Seq[Expr[G]]): Option[CPPInvocationTarget[G]] = {
    Some(RefSYCLConstructorDefinition(SYCLTSubGroup()))
  }





}
