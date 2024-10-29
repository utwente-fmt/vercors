package vct.col.ast.lang.sycl

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.resolve.ctx.{CPPInvocationTarget, RefSYCLConstructorDefinition}
import vct.col.resolve.lang.{CPP, Util}
import vct.col.ast.ops.SYCLTLocalAccessorOps

trait SYCLTLocalAccessorImpl[G] extends SYCLTLocalAccessorOps[G] {
  this: SYCLTLocalAccessor[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Text("sycl::local_accessor") <> "<" <> typ <> ", " <>
        Text(dimCount.toString) <> ">"
    )

  override val namespacePath = "sycl::local_accessor"

  def findConstructor(
      genericArgs: Seq[CPPExprOrTypeSpecifier[G]],
      args: Seq[Expr[G]],
  ): Option[CPPInvocationTarget[G]] =
    genericArgs match {
      case Seq(
            CPPExprOrTypeSpecifier(None, Some(typeSpec)),
            CPPExprOrTypeSpecifier(Some(CIntegerValue(dim)), None),
          )
          if dim > 0 && dim <= 3 && Util.compatTypes[G](
            args,
            Seq(SYCLTRange[G](dim.toInt), SYCLTHandler[G]()),
          ) =>
        Some(RefSYCLConstructorDefinition(SYCLTLocalAccessor[G](
          CPP.getBaseTypeFromSpecs[G](Seq(typeSpec)),
          dim.toInt,
        )))
      case Seq(CPPExprOrTypeSpecifier(None, Some(typeSpec)))
          if Util.compatTypes(args, Seq(SYCLTRange[G](1), SYCLTHandler[G]())) =>
        Some(RefSYCLConstructorDefinition(
          SYCLTLocalAccessor(CPP.getBaseTypeFromSpecs(Seq(typeSpec)), 1)
        ))
      case _ => None
    }
}
