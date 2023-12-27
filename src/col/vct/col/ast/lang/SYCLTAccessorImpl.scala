package vct.col.ast.lang

import vct.col.ast._
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.resolve.ctx.{CPPInvocationTarget, RefSYCLAccessMode, RefSYCLConstructorDefinition}
import vct.col.resolve.lang.{CPP, Util}

trait SYCLTAccessorImpl[G] { this: SYCLTAccessor[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("sycl::accessor") <> "<" <> typ <> ", " <> Text(dimCount.toString) <> ", " <> Text(if (readOnly) "sycl::access_mode::read" else "sycl::access_mode::read_write") <> ">")

  override val namespacePath = "sycl::accessor"

  def findConstructor(genericArgs: Seq[CPPExprOrTypeSpecifier[G]], args: Seq[Expr[G]]): Option[CPPInvocationTarget[G]] = {
    if (args.nonEmpty) CPP.unwrappedType(args.head.t) match {
      case SYCLTBuffer(typ, dimCount) => genericArgs match {
        case Seq(CPPExprOrTypeSpecifier(None, Some(typeSpec)), CPPExprOrTypeSpecifier(Some(CIntegerValue(dim)), None), CPPExprOrTypeSpecifier(None, Some(SYCLClassDefName("access_mode::read_write", Nil)))) if dim > 0 && dim <= 3 &&
          Util.compatTypes[G](args, Seq(SYCLTBuffer(CPP.getBaseTypeFromSpecs(Seq(typeSpec)), dim.toInt), SYCLTHandler[G](), SYCLTAccessMode[G]())) && isAccessMode(args(2), SYCLReadWriteAccess[G]()) =>
          Some(RefSYCLConstructorDefinition(SYCLTAccessor(typ, dimCount)))
        case Seq(CPPExprOrTypeSpecifier(None, Some(typeSpec)), CPPExprOrTypeSpecifier(Some(CIntegerValue(dim)), None), CPPExprOrTypeSpecifier(None, Some(SYCLClassDefName("access_mode::read", Nil)))) if dim > 0 && dim <= 3 &&
          Util.compatTypes[G](args, Seq(SYCLTBuffer(CPP.getBaseTypeFromSpecs(Seq(typeSpec)), dim.toInt), SYCLTHandler[G](), SYCLTAccessMode[G]())) && isAccessMode(args(2), SYCLReadOnlyAccess[G]()) =>
          Some(RefSYCLConstructorDefinition(SYCLTAccessor(typ, dimCount, readOnly = true)))
        case Seq(CPPExprOrTypeSpecifier(None, Some(typeSpec)), CPPExprOrTypeSpecifier(Some(CIntegerValue(dim)), None)) if dim > 0 && dim <= 3 &&
          Util.compatTypes[G](args, Seq(SYCLTBuffer(CPP.getBaseTypeFromSpecs(Seq(typeSpec)), dim.toInt), SYCLTHandler[G](), SYCLTAccessMode[G]())) && isAccessMode(args(2), SYCLReadWriteAccess[G]()) =>
          Some(RefSYCLConstructorDefinition(SYCLTAccessor(typ, dimCount)))
        case Seq(CPPExprOrTypeSpecifier(None, Some(typeSpec))) if dimCount == 1 &&
          Util.compatTypes[G](args, Seq(SYCLTBuffer(CPP.getBaseTypeFromSpecs(Seq(typeSpec))), SYCLTHandler[G](), SYCLTAccessMode[G]())) && isAccessMode(args(2), SYCLReadWriteAccess[G]()) =>
          Some(RefSYCLConstructorDefinition(SYCLTAccessor(typ, dimCount)))
        case Nil => Some(RefSYCLConstructorDefinition(SYCLTAccessor(typ, dimCount, isAccessMode(args(2), SYCLReadOnlyAccess[G]()))))
        case _ => None
      }
      case _ => None
    } else {
      None
    }
  }

  private def isAccessMode(e: Expr[G], accessMode: SYCLAccessMode[G]): Boolean = e match {
    case local: CPPLocal[G] if local.ref.get.equals(RefSYCLAccessMode(accessMode)) => true
    case _ => false
  }
}