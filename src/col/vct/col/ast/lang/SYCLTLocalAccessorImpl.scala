package vct.col.ast.lang

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.resolve.ctx.{CPPInvocationTarget, RefSYCLConstructorDefinition}
import vct.col.resolve.lang.{CPP, Util}

trait SYCLTLocalAccessorImpl[G] { this: SYCLTLocalAccessor[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::local_accessor") <> "<" <> typ <> ", " <> Text(dimCount.toString) <> ">")

  override val namespacePath = "sycl::local_accessor"

  def findConstructor(genericArgs: Seq[CPPExprOrTypeSpecifier[G]], args: Seq[Expr[G]]): Option[CPPInvocationTarget[G]] = genericArgs match {
    case Seq(CPPExprOrTypeSpecifier(None, Some(typeSpec)), CPPExprOrTypeSpecifier(Some(IntegerValue(dim)), None)) if dim > 0 && dim <= 3 &&
      Util.compatTypes[G](args, Seq(SYCLTRange[G](dim.toInt), SYCLTHandler[G]())) => Some(RefSYCLConstructorDefinition(SYCLTLocalAccessor[G](CPP.getBaseTypeFromSpecs[G](Seq(typeSpec)), dim.toInt)))
    case Seq(CPPExprOrTypeSpecifier(None, Some(typeSpec))) if Util.compatTypes(args, Seq(SYCLTRange[G](1), SYCLTHandler[G]())) =>
      Some(RefSYCLConstructorDefinition(SYCLTBuffer(CPP.getBaseTypeFromSpecs(Seq(typeSpec)), 1)))
    case Seq(CPPExprOrTypeSpecifier(None, Some(typeSpec))) if Util.compatTypes(args, Seq(SYCLTRange[G](2), SYCLTHandler[G]())) =>
      Some(RefSYCLConstructorDefinition(SYCLTBuffer(CPP.getBaseTypeFromSpecs(Seq(typeSpec)), 2)))
    case Seq(CPPExprOrTypeSpecifier(None, Some(typeSpec))) if Util.compatTypes(args, Seq(SYCLTRange[G](3), SYCLTHandler[G]())) =>
      Some(RefSYCLConstructorDefinition(SYCLTBuffer(CPP.getBaseTypeFromSpecs(Seq(typeSpec)), 3)))
    case _ => None
  }
}