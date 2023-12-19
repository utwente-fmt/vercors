package vct.col.ast.lang.sycl

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.resolve.ctx.{CPPInvocationTarget, RefSYCLConstructorDefinition}
import vct.col.resolve.lang.{CPP, Util}

trait SYCLTAccessorImpl[G] { this: SYCLTAccessor[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::accessor") <> "<" <> typ <> ", " <> Text(dimCount.toString) <> ">")

  override val namespacePath = "sycl::accessor"

  def findConstructor(genericArgs: Seq[CPPExprOrTypeSpecifier[G]], args: Seq[Expr[G]]): Option[CPPInvocationTarget[G]] = genericArgs match {
    case Nil if args.nonEmpty => CPP.unwrappedType(args.head.t) match {
      case SYCLTBuffer(typ, dimCount) if Util.compatTypes(args.tail, Seq(SYCLTHandler[G](), SYCLTAccessMode[G]())) => Some(RefSYCLConstructorDefinition(SYCLTAccessor(typ, dimCount)))
      case _ => None
    }
    case _ => None
  }
}