package vct.col.resolve

import hre.util.FuncTools
import vct.col.ast.{ADTFunction, CAnonymousFunctionDeclarator, CArrayDeclarator, CDeclarationSpecifier, CDeclarator, CInt, CLong, CName, CParam, CPointerDeclarator, CPrimitiveType, CSigned, CTypedFunctionDeclarator, CUnsigned, DiagnosticOrigin, Expr, Origin, TArray, TNotAValue, TPointer, Type}

import scala.annotation.tailrec

case object C {
  implicit private val o: Origin = DiagnosticOrigin

  val NUMBER_LIKE_PREFIXES: Seq[Seq[CDeclarationSpecifier]] = Seq(
    Nil,
    Seq(CUnsigned()),
    Seq(CSigned()),
  )

  val NUMBER_LIKE_TYPES: Seq[Seq[CDeclarationSpecifier]] = Seq(
    Seq(CInt()),
    Seq(CLong()),
    Seq(CLong(), CInt()),
    Seq(CLong(), CLong()),
    Seq(CLong(), CLong(), CInt()),
  )

  val NUMBER_LIKE_SPECIFIERS: Seq[Seq[CDeclarationSpecifier]] =
    for (prefix <- NUMBER_LIKE_PREFIXES; t <- NUMBER_LIKE_TYPES)
      yield prefix ++ t

  case class DeclaratorInfo(params: Option[Seq[CParam]], typeOrReturnType: Type => Type, name: String)

  def getDeclaratorInfo(decl: CDeclarator): DeclaratorInfo = decl match {
    case CPointerDeclarator(pointers, inner) =>
      val innerInfo = getDeclaratorInfo(inner)
      DeclaratorInfo(
        innerInfo.params,
        t => FuncTools.repeat(TArray(_), pointers.size, innerInfo.typeOrReturnType(t)),
        innerInfo.name)
    case CArrayDeclarator(_, _, inner) =>
      val innerInfo = getDeclaratorInfo(inner)
      // TODO PB: I think pointer is not correct here.
      DeclaratorInfo(innerInfo.params, t => TPointer(innerInfo.typeOrReturnType(t)), innerInfo.name)
    case CTypedFunctionDeclarator(params, _, inner) =>
      val innerInfo = getDeclaratorInfo(inner)
      DeclaratorInfo(params=Some(params), typeOrReturnType=(t => t), innerInfo.name)
    case CAnonymousFunctionDeclarator(Nil, inner) =>
      val innerInfo = getDeclaratorInfo(inner)
      DeclaratorInfo(params=Some(Nil), typeOrReturnType=(t => t), innerInfo.name)
    case decl @ CAnonymousFunctionDeclarator(_, _) =>
      throw AnonymousMethodsUnsupported(decl)
    case CName(name) => DeclaratorInfo(params=None, typeOrReturnType=(t => t), name)
  }

  def nameFromDeclarator(declarator: CDeclarator): String =
    getDeclaratorInfo(declarator).name

  def typeOrReturnTypeFromDeclaration(specs: Seq[CDeclarationSpecifier], decl: CDeclarator): Type =
    getDeclaratorInfo(decl).typeOrReturnType(CPrimitiveType(specs))

  def paramsFromDeclarator(declarator: CDeclarator): Seq[CParam] =
    getDeclaratorInfo(declarator).params.get

  def findCTypeName(name: String, ctx: TypeResolutionContext): Option[CTypeNameTarget] =
    ctx.stack.flatten.collectFirst {
      case target: CTypeNameTarget if target.name == name => target
    }

  def findCName(name: String, ctx: ReferenceResolutionContext): Option[CNameTarget] =
    ctx.stack.flatten.collectFirst {
      case target: CNameTarget if target.name == name => target
    }

  def findDeref(obj: Expr, name: String, ctx: ReferenceResolutionContext): Option[CDerefTarget] =
    obj.t match {
      case t @ TNotAValue() => t.decl.get match {
        case RefAxiomaticDataType(decl) => decl.decls.flatMap(Referrable.from).collectFirst {
          case ref: RefADTFunction if ref.name == name => ref
        }
        case _ => Spec.builtinField(obj, name)
      }
    }

  def resolveInvocation(obj: Expr, ctx: ReferenceResolutionContext): CInvocationTarget =
    obj.t match {
      case t @ TNotAValue() => t.decl.get match {
        case target: CInvocationTarget => target
        case _ => throw NotApplicable(obj)
      }
      case _ => throw NotApplicable(obj)
    }
}
