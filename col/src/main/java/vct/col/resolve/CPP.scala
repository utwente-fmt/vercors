package vct.col.resolve

import hre.util.FuncTools
import vct.col.ast._
import vct.col.ast.langspecific.cpp
import vct.col.origin.{Blame, BuiltinError, DiagnosticOrigin, Origin}
import vct.col.util.Types
import vct.result.VerificationError.UserError

case object CPP {
  implicit private val o: Origin = DiagnosticOrigin

  case class CPPTypeNotSupported(node: Option[Node[_]]) extends UserError {
    override def code: String = "cppTypeNotSupported"
    override def text: String = {
      (node match {
        case Some(node) => node.o.messageInContext(_)
        case None => (text: String) => text
      })("This type is not supported by VerCors.")
    }
  }

  val NUMBER_LIKE_PREFIXES: Seq[Seq[CPPDeclarationSpecifier[_]]] = Seq(
    Nil,
    Seq(CPPUnsigned()),
    Seq(CPPSigned()),
  )

  val NUMBER_LIKE_TYPES: Seq[Seq[CPPDeclarationSpecifier[_]]] = Seq(
    Seq(CPPInt()),
    Seq(CPPLong()),
    Seq(CPPLong(), CPPInt()),
    Seq(CPPLong(), CPPLong()),
    Seq(CPPLong(), CPPLong(), CPPInt()),
  )

  val NUMBER_LIKE_SPECIFIERS: Seq[Seq[CPPDeclarationSpecifier[_]]] =
    for (prefix <- NUMBER_LIKE_PREFIXES; t <- NUMBER_LIKE_TYPES)
      yield prefix ++ t


  case class DeclaratorInfo[G](params: Option[Seq[CPPParam[G]]], typeOrReturnType: Type[G] => Type[G], name: String)

  def getDeclaratorInfo[G](decl: CPPDeclarator[G]): DeclaratorInfo[G] = decl match {
    case CPPPointerDeclarator(pointers, inner) =>
      val innerInfo = getDeclaratorInfo(inner)
      DeclaratorInfo(
        innerInfo.params,
        t => FuncTools.repeat[Type[G]](TPointer(_), pointers.size, innerInfo.typeOrReturnType(t)),
        innerInfo.name)
    case CPPArrayDeclarator(_, _, inner) =>
      val innerInfo = getDeclaratorInfo(inner)
      // TODO PB: I think pointer is not correct here.
      // TODO OS: See how PB resolved this in current dev
      DeclaratorInfo(innerInfo.params, t => TPointer(innerInfo.typeOrReturnType(t)), innerInfo.name)
    case CPPTypedFunctionDeclarator(params, _, inner) =>
      val innerInfo = getDeclaratorInfo(inner)
      DeclaratorInfo(params=Some(params), typeOrReturnType=(t => t), innerInfo.name)
    case CPPAnonymousFunctionDeclarator(Nil, inner) =>
      val innerInfo = getDeclaratorInfo(inner)
      DeclaratorInfo(params=Some(Nil), typeOrReturnType=(t => t), innerInfo.name)
    case decl @ CPPAnonymousFunctionDeclarator(_, _) =>
      throw AnonymousCPPMethodsUnsupported(decl)
    case CPPName(name) => DeclaratorInfo(params=None, typeOrReturnType=(t => t), name)
  }



  def getPrimitiveType[G](specs: Seq[CPPDeclarationSpecifier[G]], context: Option[Node[G]] = None): Type[G] =
    specs.collect { case spec: CPPTypeSpecifier[G] => spec } match {
      case Seq(CPPVoid()) => TVoid()
      case Seq(CPPChar()) => TChar()
      case t if CPP.NUMBER_LIKE_SPECIFIERS.contains(t) => TInt()
      case Seq(CPPFloat()) | Seq(CPPDouble()) | Seq(CPPLong(), CPPDouble()) => TFloat()
      case Seq(CPPBool()) => TBool()
      case Seq(defn @ CPPTypedefName(_)) => Types.notAValue(defn.ref.get)
      case _ => throw CPPTypeNotSupported(context)
    }

  def nameFromDeclarator(declarator: CPPDeclarator[_]): String =
    getDeclaratorInfo(declarator).name

  def typeOrReturnTypeFromDeclaration[G](specs: Seq[CPPDeclarationSpecifier[G]], decl: CPPDeclarator[G]): Type[G] =
    getDeclaratorInfo(decl).typeOrReturnType(CPPPrimitiveType(specs))

  def paramsFromDeclarator[G](declarator: CPPDeclarator[G]): Seq[CPPParam[G]] =
    getDeclaratorInfo(declarator).params.get

  def findCPPTypeName[G](name: String, ctx: TypeResolutionContext[G]): Option[CPPTypeNameTarget[G]] =
    ctx.stack.flatten.collectFirst {
      case target: CPPTypeNameTarget[G] if target.name == name => target
    }

  def findCPPName[G](name: String, ctx: ReferenceResolutionContext[G]): Option[CPPNameTarget[G]] =
    ctx.stack.flatten.collectFirst {
      case target: CPPNameTarget[G] if target.name == name => target
    }

  def findDeref[G](obj: Expr[G], name: String, ctx: ReferenceResolutionContext[G], blame: Blame[BuiltinError]): Option[CPPDerefTarget[G]] =
    obj.t match {
      case t: TNotAValue[G] => t.decl.get match {
        case RefAxiomaticDataType(decl) => decl.decls.flatMap(Referrable.from).collectFirst {
          case ref: RefADTFunction[G] if ref.name == name => ref
        }
        case _ => Spec.builtinField(obj, name, blame)
      }
      case _ => Spec.builtinField(obj, name, blame)
    }

  def resolveInvocation[G](obj: Expr[G], ctx: ReferenceResolutionContext[G]): CPPInvocationTarget[G] =
    obj.t match {
      case t: TNotAValue[G] => t.decl.get match {
        case target: CPPInvocationTarget[G] => target
        case _ => throw NotApplicable(obj)
      }
      case _ => throw NotApplicable(obj)
    }

}
