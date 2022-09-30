package vct.col.resolve.lang

import hre.util.FuncTools
import vct.col.ast._
import vct.col.ast.`type`.TFloats
import vct.col.origin._
import vct.col.resolve._
import vct.col.resolve.ctx._
import vct.col.typerules.Types
import vct.result.VerificationError.UserError

case object C {
  implicit private val o: Origin = DiagnosticOrigin

  case class CTypeNotSupported(node: Option[Node[_]]) extends UserError {
    override def code: String = "cTypeNotSupported"
    override def text: String = {
      (node match {
        case Some(node) => node.o.messageInContext(_)
        case None => (text: String) => text
      })("This type is not supported by VerCors.")
    }
  }

  val NUMBER_LIKE_PREFIXES: Seq[Seq[CDeclarationSpecifier[_]]] = Seq(
    Nil,
    Seq(CUnsigned()),
    Seq(CSigned()),
  )

  val NUMBER_LIKE_TYPES: Seq[Seq[CDeclarationSpecifier[_]]] = Seq(
    Seq(CInt()),
    Seq(CLong()),
    Seq(CLong(), CInt()),
    Seq(CLong(), CLong()),
    Seq(CLong(), CLong(), CInt()),
  )

  val NUMBER_LIKE_SPECIFIERS: Seq[Seq[CDeclarationSpecifier[_]]] =
    for (prefix <- NUMBER_LIKE_PREFIXES; t <- NUMBER_LIKE_TYPES)
      yield prefix ++ t

  case class DeclaratorInfo[G](params: Option[Seq[CParam[G]]], typeOrReturnType: Type[G] => Type[G], name: String)

  def getDeclaratorInfo[G](decl: CDeclarator[G]): DeclaratorInfo[G] = decl match {
    case CPointerDeclarator(pointers, inner) =>
      val innerInfo = getDeclaratorInfo(inner)
      DeclaratorInfo(
        innerInfo.params,
        t => FuncTools.repeat[Type[G]](TPointer(_), pointers.size, innerInfo.typeOrReturnType(t)),
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

  def getPrimitiveType[G](specs: Seq[CDeclarationSpecifier[G]], context: Option[Node[G]] = None): Type[G] =
    specs.collect { case spec: CTypeSpecifier[G] => spec } match {
      case Seq(CVoid()) => TVoid()
      case Seq(CChar()) => TChar()
      case t if C.NUMBER_LIKE_SPECIFIERS.contains(t) => TInt()
      case Seq(CFloat()) => TFloats.ieee754_32bit
      case Seq(CDouble()) => TFloats.ieee754_64bit
      case Seq(CLong(), CDouble()) => TFloats.x86extended_80bit
      case Seq(CBool()) => TBool()
      case Seq(defn @ CTypedefName(_)) => Types.notAValue(defn.ref.get)
      case _ => throw CTypeNotSupported(context)
    }

  def nameFromDeclarator(declarator: CDeclarator[_]): String =
    getDeclaratorInfo(declarator).name

  def typeOrReturnTypeFromDeclaration[G](specs: Seq[CDeclarationSpecifier[G]], decl: CDeclarator[G]): Type[G] =
    getDeclaratorInfo(decl).typeOrReturnType(CPrimitiveType(specs))

  def paramsFromDeclarator[G](declarator: CDeclarator[G]): Seq[CParam[G]] =
    getDeclaratorInfo(declarator).params.get

  def findCTypeName[G](name: String, ctx: TypeResolutionContext[G]): Option[CTypeNameTarget[G]] =
    ctx.stack.flatten.collectFirst {
      case target: CTypeNameTarget[G] if target.name == name => target
    }

  def findCName[G](name: String, ctx: ReferenceResolutionContext[G]): Option[CNameTarget[G]] =
    name match {
      case "threadIdx" => Some(RefCudaThreadIdx())
      case "blockDim" => Some(RefCudaBlockDim())
      case "blockIdx" => Some(RefCudaBlockIdx())
      case "gridDim" => Some(RefCudaGridDim())
      case _ => ctx.stack.flatten.collectFirst {
        case target: CNameTarget[G] if target.name == name => target
      }
    }

  def findForwardDeclaration[G](declarator: CDeclarator[G], ctx: ReferenceResolutionContext[G]): Option[RefCGlobalDeclaration[G]] =
    ctx.stack.flatten.collectFirst {
      case target: RefCGlobalDeclaration[G] if target.name == nameFromDeclarator(declarator) => target
    }

  def findDefinition[G](declarator: CDeclarator[G], ctx: ReferenceResolutionContext[G]): Option[RefCFunctionDefinition[G]] =
    ctx.stack.flatten.collectFirst {
      case target: RefCFunctionDefinition[G] if target.name == nameFromDeclarator(declarator) => target
    }

  def findDeref[G](obj: Expr[G], name: String, ctx: ReferenceResolutionContext[G], blame: Blame[BuiltinError]): Option[CDerefTarget[G]] =
    (obj.t match {
      case t: TNotAValue[G] => t.decl.get match {
        case RefAxiomaticDataType(decl) => decl.decls.flatMap(Referrable.from).collectFirst {
          case ref: RefADTFunction[G] if ref.name == name => ref
        }
        case _ => None
      }
      case CTCudaVec() =>
        val ref = obj.asInstanceOf[CLocal[G]].ref.get.asInstanceOf[RefCudaVec[G]]
        name match {
          case "x" => Some(RefCudaVecX(ref))
          case "y" => Some(RefCudaVecY(ref))
          case "z" => Some(RefCudaVecZ(ref))
          case _ => None
        }
      case _ => None
    }).orElse(Spec.builtinField(obj, name, blame))

  def resolveInvocation[G](obj: Expr[G], ctx: ReferenceResolutionContext[G]): CInvocationTarget[G] =
    obj.t match {
      case t: TNotAValue[G] => t.decl.get match {
        case target: CInvocationTarget[G] => target
        case _ => throw NotApplicable(obj)
      }
      case _ => throw NotApplicable(obj)
    }
}
