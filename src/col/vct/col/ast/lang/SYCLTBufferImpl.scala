package vct.col.ast.lang

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.resolve.ctx.{CPPInvocationTarget, RefSYCLConstructorDefinition}
import vct.col.resolve.lang.{CPP, Util}

trait SYCLTBufferImpl[G] { this: SYCLTBuffer[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::buffer") <> "<" <> typ <> ", " <> Text(dimCount.toString) <> ">")

  override val namespacePath = "sycl::buffer"

  def findConstructor(genericArgs: Seq[CPPExprOrTypeSpecifier[G]], args: Seq[Expr[G]]): Option[CPPInvocationTarget[G]] = genericArgs match {
    case Seq(CPPExprOrTypeSpecifier(None, Some(typeSpec)), CPPExprOrTypeSpecifier(Some(IntegerValue(dim)), None)) if dim > 0 && dim <= 3 &&
      Util.compatTypes[G](args, Seq(TPointer[G](CPP.getBaseTypeFromSpecs[G](Seq(typeSpec))), SYCLTRange[G](dim.toInt))) => Some(RefSYCLConstructorDefinition[G](SYCLTBuffer[G](CPP.getBaseTypeFromSpecs[G](Seq(typeSpec)), dim.toInt)))
    case Seq(CPPExprOrTypeSpecifier(None, Some(typeSpec))) if Util.compatTypes(args, Seq(TPointer[G](CPP.getBaseTypeFromSpecs(Seq(typeSpec))), SYCLTRange[G](1))) =>
      Some(RefSYCLConstructorDefinition(SYCLTBuffer(CPP.getBaseTypeFromSpecs(Seq(typeSpec)), 1)))
    case Seq(CPPExprOrTypeSpecifier(None, Some(typeSpec))) if Util.compatTypes(args, Seq(TPointer[G](CPP.getBaseTypeFromSpecs(Seq(typeSpec))), SYCLTRange[G](2))) =>
      Some(RefSYCLConstructorDefinition(SYCLTBuffer(CPP.getBaseTypeFromSpecs(Seq(typeSpec)), 2)))
    case Seq(CPPExprOrTypeSpecifier(None, Some(typeSpec))) if Util.compatTypes(args, Seq(TPointer[G](CPP.getBaseTypeFromSpecs(Seq(typeSpec))), SYCLTRange[G](3))) =>
      Some(RefSYCLConstructorDefinition(SYCLTBuffer(CPP.getBaseTypeFromSpecs(Seq(typeSpec)), 3)))
    case Nil if args.nonEmpty && args.head.t.asPointer.isDefined && Util.compatTypes(args.tail, Seq(SYCLTRange[G](1))) => Some(RefSYCLConstructorDefinition(SYCLTBuffer(args.head.t.asPointer.get.element, 1)))
    case Nil if args.nonEmpty && args.head.t.asPointer.isDefined && Util.compatTypes(args.tail, Seq(SYCLTRange[G](2))) => Some(RefSYCLConstructorDefinition(SYCLTBuffer(args.head.t.asPointer.get.element, 2)))
    case Nil if args.nonEmpty && args.head.t.asPointer.isDefined && Util.compatTypes(args.tail, Seq(SYCLTRange[G](3))) => Some(RefSYCLConstructorDefinition(SYCLTBuffer(args.head.t.asPointer.get.element, 3)))
    case _ => None
  }
}