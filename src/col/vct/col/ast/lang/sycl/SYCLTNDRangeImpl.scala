package vct.col.ast.lang.sycl

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.resolve.ctx.{CPPInvocationTarget, RefSYCLConstructorDefinition}
import vct.col.resolve.lang.Util
import vct.col.ast.ops.SYCLTNDRangeOps

trait SYCLTNDRangeImpl[G] extends SYCLTNDRangeOps[G] { this: SYCLTNDRange[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::nd_range") <> "<" <> Text(dimCount.toString) <> ">")

  override val namespacePath = "sycl::nd_range"

  def findConstructor(genericArgs: Seq[CPPExprOrTypeSpecifier[G]], args: Seq[Expr[G]]): Option[CPPInvocationTarget[G]] = genericArgs match {
    case Seq(CPPExprOrTypeSpecifier(Some(CIntegerValue(dim)), None)) if dim > 0 && dim <= 3 && Util.compatTypes(args, Seq(SYCLTRange[G](dim.toInt), SYCLTRange[G](dim.toInt))) => Some(RefSYCLConstructorDefinition(SYCLTNDRange(dim.toInt)))
    case Nil if Util.compatTypes(args, Seq(SYCLTRange[G](1), SYCLTRange[G](1))) => Some(RefSYCLConstructorDefinition(SYCLTNDRange(1)))
    case Nil if Util.compatTypes(args, Seq(SYCLTRange[G](2), SYCLTRange[G](2))) => Some(RefSYCLConstructorDefinition(SYCLTNDRange(2)))
    case Nil if Util.compatTypes(args, Seq(SYCLTRange[G](3), SYCLTRange[G](3))) => Some(RefSYCLConstructorDefinition(SYCLTNDRange(3)))
    case _ => None
  }
}