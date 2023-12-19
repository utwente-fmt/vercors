package vct.col.ast.lang.sycl

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.resolve.ctx.{CPPInvocationTarget, RefSYCLConstructorDefinition}
import vct.col.resolve.lang.Util

trait SYCLTRangeImpl[G] { this: SYCLTRange[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::range") <> "<" <> Text(dimCount.toString) <> ">")

  val namespacePath = "sycl::range"

  def findConstructor(genericArgs: Seq[CPPExprOrTypeSpecifier[G]], args: Seq[Expr[G]]): Option[CPPInvocationTarget[G]] = genericArgs match {
    case Seq(CPPExprOrTypeSpecifier(Some(CIntegerValue(dim)), None)) if dim > 0 && dim <= 3 && Util.compatTypes(args, Seq.range(0, dim.toInt).map(_ => TCInt[G]())) => Some(RefSYCLConstructorDefinition(SYCLTRange(dim.toInt)))
    case Nil if Util.compatTypes(args, Seq(TCInt[G]())) => Some(RefSYCLConstructorDefinition(SYCLTRange(1)))
    case Nil if Util.compatTypes(args, Seq(TCInt[G](), TCInt[G]())) => Some(RefSYCLConstructorDefinition(SYCLTRange(2)))
    case Nil if Util.compatTypes(args, Seq(TCInt[G](), TCInt[G](), TCInt[G]())) => Some(RefSYCLConstructorDefinition(SYCLTRange(3)))
    case _ => None
  }
}