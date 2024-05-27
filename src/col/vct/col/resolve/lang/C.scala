package vct.col.resolve.lang

import hre.util.FuncTools
import vct.col.ast._
import vct.col.ast.`type`.typeclass.TFloats.{C_ieee754_32bit, C_ieee754_64bit}
import vct.col.ast.util.ExpressionEqualityCheck.isConstantInt
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

  val INTEGER_LIKE_TYPES: Seq[Seq[CDeclarationSpecifier[_]]] = Seq(
    Seq(CShort()),
    Seq(CShort(), CInt()),
    Seq(CInt()),
    Seq(CLong()),
    Seq(CLong(), CInt()),
    Seq(CLong(), CLong()),
    Seq(CLong(), CLong(), CInt()),
  )

  // See here for more discussion https://github.com/utwente-fmt/vercors/discussions/1018#discussioncomment-5966388
  sealed trait DataModel
  case object ILP32 extends DataModel
  case object LP64 extends DataModel
  case object LLP64 extends DataModel

  def INT_TYPE_TO_SIZE(dm: DataModel): Map[Seq[CDeclarationSpecifier[_]], Int] = Map(
    (Seq(CShort()) -> 16),
    (Seq(CShort(), CInt()) -> 16),
    (Seq(CInt()) -> 32),
    (Seq(CLong()) -> 64),
    (Seq(CLong(), CInt()) -> (if(dm == LP64) 64 else 32)),
    (Seq(CLong(), CLong()) -> 64),
    (Seq(CLong(), CLong(), CInt()) -> 64),
  )

  case class DeclaratorInfo[G](params: Option[Seq[CParam[G]]], typeOrReturnType: Type[G] => Type[G], name: String)

  def getDeclaratorInfo[G](decl: CDeclarator[G]): DeclaratorInfo[G] = decl match {
    case CPointerDeclarator(pointers, inner) =>
      val innerInfo = getDeclaratorInfo(inner)
      DeclaratorInfo(
        innerInfo.params,
        t => innerInfo.typeOrReturnType(FuncTools.repeat[Type[G]](CTPointer(_), pointers.size, t)),
        innerInfo.name)
    case c @ CArrayDeclarator(_, size, inner) =>
      val innerInfo = getDeclaratorInfo(inner)
      DeclaratorInfo(innerInfo.params, t => innerInfo.typeOrReturnType(CTArray(size, t)(c.blame)), innerInfo.name)
    case CPointerDeclarator(pointers, inner) =>
      val innerInfo = getDeclaratorInfo(inner)
      DeclaratorInfo(
        innerInfo.params,
        t => innerInfo.typeOrReturnType(FuncTools.repeat[Type[G]](CTPointer(_), pointers.size, t)),
        innerInfo.name)
    case CTypeExtensionDeclarator(Seq(CTypeAttribute(name, Seq(size))), inner) if name == "vector_size" || name == "__vector_size__" =>
      val innerInfo = getDeclaratorInfo(inner)
      DeclaratorInfo(
        innerInfo.params,
        t => CTVector(size, innerInfo.typeOrReturnType(t))(decl.o),
        innerInfo.name)
    case typeExtension@CTypeExtensionDeclarator(_, _) => throw CTypeNotSupported(Some(typeExtension))
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

  def getSpecs[G](decl: CDeclarator[G], acc: Seq[CDeclarationSpecifier[G]] = Nil): Seq[CDeclarationSpecifier[G]] = decl match {
    case CTypeExtensionDeclarator(extensions, inner) => getSpecs(inner, acc :+ CFunctionTypeExtensionModifier(extensions))
    case _ => acc
  }

  def getTypeFromTypeDef[G](decl: CDeclaration[G], context: Option[Node[G]] = None): Type[G] = {
    val specs: Seq[CDeclarationSpecifier[G]] = decl.specs match {
      case CTypedef() +: remaining => remaining
      case _ => ???
    }

    // Need to get specifications from the init (can only have one init as typedef), since it can contain GCC Type extensions
    getPrimitiveType(getSpecs(decl.inits.head.decl) ++ specs, context)
  }


  def getPrimitiveType[G](specs: Seq[CDeclarationSpecifier[G]], context: Option[Node[G]] = None): Type[G] = {
    val vectorSize: Option[Expr[G]] = specs.collect { case ext: CFunctionTypeExtensionModifier[G] => ext.extensions} match {
      case Seq(Seq(CTypeAttribute(name, Seq(size: Expr[G])))) if name == "__vector_size__" || name == "vector_size" => Some(size)
      case Seq() => None
      case _ => throw CTypeNotSupported(context)
    }

    val filteredSpecs = specs.filter { case _: CFunctionTypeExtensionModifier[G] => false; case _ => true }
      .collect { case spec: CTypeSpecifier[G] => spec }

    val t: Type[G] = filteredSpecs match {
      case Seq(CVoid()) => TVoid()
      case Seq(CChar()) => TChar()
      case CUnsigned() +: t if INTEGER_LIKE_TYPES.contains(t) => TCInt()
      case CSigned() +: t if INTEGER_LIKE_TYPES.contains(t) => TCInt()
      case t if C.INTEGER_LIKE_TYPES.contains(t) => TCInt()
      case Seq(CFloat()) => C_ieee754_32bit()
      case Seq(CDouble()) => C_ieee754_64bit()
      case Seq(CLong(), CDouble()) => C_ieee754_64bit()
      case Seq(CBool()) => TBool()
      case Seq(defn @ CTypedefName(_)) =>
        defn.ref.get match {
          case RefTypeDef(decl) => getTypeFromTypeDef(decl.decl)
          case _ => ???
        }
      case Seq(CSpecificationType(typ)) => typ
      case Seq(defn @ CStructSpecifier(_)) => CTStruct(defn.ref.get.decl.ref)
      case spec +: _ => throw CTypeNotSupported(context.orElse(Some(spec)))
      case _ => throw CTypeNotSupported(context)
    }
    vectorSize match {
      case None => t
      case Some(size) => CTVector(size, t)
    }
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

  def findCStruct[G](name: String, ctx: TypeResolutionContext[G]): Option[RefCStruct[G]] =
    ctx.stack.flatten.collectFirst {
      case target: RefCStruct[G] if target.name == name => target
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

  def stripCPrimitiveType[G](t: Type[G]): Type[G] = t match {
    case CPrimitiveType(specs) => getPrimitiveType(specs)
    case _ => t
  }


  def findPointerDeref[G](obj: Expr[G], name: String, ctx: ReferenceResolutionContext[G], blame: Blame[BuiltinError]): Option[CDerefTarget[G]] =
    stripCPrimitiveType(obj.t) match {
      case CTPointer(innerType: TNotAValue[G]) => innerType.decl.get match {
        case RefCStruct(decl) => getCStructDeref(decl, name)
        case _ => None
      }
      case CTPointer(struct: CTStruct[G]) =>
        getCStructDeref(struct.ref.decl, name)
      case CTArray(_, innerType: TNotAValue[G]) => innerType.decl.get match {
        case RefCStruct(decl) => getCStructDeref(decl, name)
        case _ => None
      }
      case CTArray(_, struct: CTStruct[G]) =>
        getCStructDeref(struct.ref.decl, name)
      case _ => None
    }

  def getCStructDeref[G](decl: CGlobalDeclaration[G], name: String): Option[CDerefTarget[G]] =
    decl.decl match {
      case CDeclaration(_, _, Seq(CStructDeclaration(_, decls)), Seq()) =>
        decls.flatMap(Referrable.from).collectFirst {
          case ref: RefCStructField[G] if ref.name == name => ref
        }
      case _ => None
    }

  def openCLVectorAccessString[G](access: String, typeSize: BigInt): Option[Seq[BigInt]] = access match {
    case "lo" if typeSize % 2 == 0 => Some(Seq.tabulate(typeSize.toInt/2)(i => i))
    case "hi" if typeSize % 2 == 0 => Some(Seq.tabulate(typeSize.toInt/2)(i => i + typeSize/2))
    case "even" if typeSize % 2 == 0 => Some(Seq.tabulate(typeSize.toInt/2)(i => 2*i))
    case "odd" if typeSize % 2 == 0 => Some(Seq.tabulate(typeSize.toInt/2)(i => 2*i+1))
    case s if s.head == 's' && s.tail.nonEmpty =>
      val hexToInt = (i: Char) => i match {
        case i if i.isDigit => BigInt(i - '0'.toInt)
        case i if i >= 'a' && i <= 'f' => BigInt(i.toInt - 'a'.toInt + 10)
        case i if i >= 'A' && i <= 'F' => BigInt(i.toInt - 'A'.toInt + 10)
        case _ => return None
      }
      val res = access.tail.map(hexToInt)
      if(res.forall(p => p < typeSize))
        Some(res)
      else
        None
    case _ =>
      val xyzwToInt = (i: Char) => i match {
        case 'x' => BigInt(0)
        case 'y' => BigInt(1)
        case 'z' => BigInt(2)
        case 'w' => BigInt(3)
        case _ => return None
      }
      val res = access.map(xyzwToInt)
      if(res.forall(p => p < typeSize))
        Some(res)
      else
        None
  }

  def findDeref[G](obj: Expr[G], name: String, ctx: ReferenceResolutionContext[G], blame: Blame[BuiltinError]): Option[CDerefTarget[G]] =
    (stripCPrimitiveType(obj.t) match {
      case t: TNotAValue[G] => t.decl.get match {
        case RefAxiomaticDataType(decl) => decl.decls.flatMap(Referrable.from).collectFirst {
          case ref: RefADTFunction[G] if ref.name == name => ref
        }
        case RefCStruct(decl: CGlobalDeclaration[G]) => getCStructDeref(decl, name)
        case _ => None
      }
      case struct: CTStruct[G] =>
        getCStructDeref(struct.ref.decl, name)
      case CTCudaVec() =>
        val ref = obj.asInstanceOf[CLocal[G]].ref.get.asInstanceOf[RefCudaVec[G]]
        name match {
          case "x" => Some(RefCudaVecX(ref))
          case "y" => Some(RefCudaVecY(ref))
          case "z" => Some(RefCudaVecZ(ref))
          case _ => None
        }
      case v: TOpenCLVector[G] =>
        openCLVectorAccessString(name, v.size).map(RefOpenCLVectorMembers[G])
      case _ => None
    }).orElse(Spec.builtinField(obj, name, blame))

  def resolveInvocation[G](obj: Expr[G], ctx: ReferenceResolutionContext[G]): CInvocationTarget[G] =
    obj.t match {
      case t: TNotAValue[G] => t.decl.get match {
        case target: CInvocationTarget[G] => target
        case _ => throw NotApplicable(obj)
      }
      // OpenCL overloads vector literals as function invocations..
      case CPrimitiveType(CSpecificationType(v: TOpenCLVector[G]) +: _) => RefOpenCLVectorLiteralCInvocationTarget[G](v.size, v.innerType)
      case _ => throw NotApplicable(obj)
    }
}
