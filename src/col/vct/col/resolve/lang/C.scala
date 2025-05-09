package vct.col.resolve.lang

import hre.util.FuncTools
import vct.col.ast._
import vct.col.ast.`type`.typeclass.TFloats.{C_ieee754_32bit, C_ieee754_64bit}
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.resolve._
import vct.col.resolve.ctx._
import vct.col.typerules.{PlatformContext, TypeSize}
import vct.result.VerificationError.{SystemError, UserError}

import scala.annotation.tailrec

case object C {
  implicit private val o: Origin = DiagnosticOrigin

  private case class CTypeNotSupported(node: Option[Node[_]])
      extends UserError {
    override def code: String = "cTypeNotSupported"
    override def text: String = {
      (node match {
        case Some(node) => node.o.messageInContext(_)
        case None => (text: String) => text
      })("This type is not supported by VerCors.")
    }
  }

  private case class UnresolvedCType(specifiers: Seq[CDeclarationSpecifier[_]])
      extends SystemError {
    override def text: String =
      s"Attempt to determine C type '$specifiers' which needs the PlatformContext to be resolved"
  }

  val NUMBER_LIKE_PREFIXES: Seq[Seq[CDeclarationSpecifier[_]]] = Seq(
    Nil,
    Seq(CUnsigned()),
    Seq(CSigned()),
  )

  val INTEGER_LIKE_TYPES: Seq[Seq[CDeclarationSpecifier[_]]] = Seq(
    Seq(CChar()),
    Seq(CSigned(), CChar()),
    Seq(CUnsigned(), CChar()),
    Seq(CShort()),
    Seq(CSigned(), CShort()),
    Seq(CUnsigned(), CShort()),
    Seq(CShort(), CInt()),
    Seq(CSigned(), CShort(), CInt()),
    Seq(CUnsigned(), CShort(), CInt()),
    Seq(CInt()),
    Seq(CSigned()),
    Seq(CUnsigned()),
    Seq(CSigned(), CInt()),
    Seq(CUnsigned(), CInt()),
    Seq(CLong()),
    Seq(CSigned(), CLong()),
    Seq(CUnsigned(), CLong()),
    Seq(CLong(), CInt()),
    Seq(CSigned(), CLong(), CInt()),
    Seq(CUnsigned(), CLong(), CInt()),
    Seq(CLong(), CLong()),
    Seq(CSigned(), CLong(), CLong()),
    Seq(CUnsigned(), CLong(), CLong()),
    Seq(CLong(), CLong(), CInt()),
    Seq(CSigned(), CLong(), CLong(), CInt()),
    Seq(CUnsigned(), CLong(), CLong(), CInt()),
  )

  def INT_TYPE_TO_SIZE(
      platformContext: PlatformContext
  ): Map[Seq[CDeclarationSpecifier[_]], TypeSize] = {
    Map(
      (Seq(CChar()) -> platformContext.charSize),
      (Seq(CSigned(), CChar()) -> platformContext.charSize),
      (Seq(CUnsigned(), CChar()) -> platformContext.charSize),
      (Seq(CShort()) -> platformContext.shortSize),
      (Seq(CSigned(), CShort()) -> platformContext.shortSize),
      (Seq(CUnsigned(), CShort()) -> platformContext.shortSize),
      (Seq(CShort(), CInt()) -> platformContext.shortSize),
      (Seq(CSigned(), CShort(), CInt()) -> platformContext.shortSize),
      (Seq(CUnsigned(), CShort(), CInt()) -> platformContext.shortSize),
      (Seq(CInt()) -> platformContext.intSize),
      (Seq(CSigned()) -> platformContext.intSize),
      (Seq(CUnsigned()) -> platformContext.intSize),
      (Seq(CSigned(), CInt()) -> platformContext.intSize),
      (Seq(CUnsigned(), CInt()) -> platformContext.intSize),
      (Seq(CLong()) -> platformContext.longSize),
      (Seq(CSigned(), CLong()) -> platformContext.longSize),
      (Seq(CUnsigned(), CLong()) -> platformContext.longSize),
      (Seq(CLong(), CInt()) -> platformContext.longSize),
      (Seq(CSigned(), CLong(), CInt()) -> platformContext.longSize),
      (Seq(CUnsigned(), CLong(), CInt()) -> platformContext.longSize),
      (Seq(CLong(), CLong()) -> platformContext.longLongSize),
      (Seq(CSigned(), CLong(), CLong()) -> platformContext.longLongSize),
      (Seq(CUnsigned(), CLong(), CLong()) -> platformContext.longLongSize),
      (Seq(CLong(), CLong(), CInt()) -> platformContext.longLongSize),
      (Seq(CSigned(), CLong(), CLong(), CInt()) ->
        platformContext.longLongSize),
      (Seq(CUnsigned(), CLong(), CLong(), CInt()) ->
        platformContext.longLongSize),
    )
  }

  private def getIntSize(
      platformContext: PlatformContext,
      specs: Seq[CDeclarationSpecifier[_]],
  ): TypeSize = {
    specs.collectFirst { case CSpecificationType(t) => t.bits } match {
      case Some(size: TypeSize.Exact) => size
      case None =>
        INT_TYPE_TO_SIZE(platformContext).getOrElse(
          specs.flatMap(_ match {
            // Inline/Pure
            case _: CSpecificationModifier[_] => Nil
            // Extern/static/typedef/...
            case _: CStorageClassSpecifier[_] => Nil
            // Actual types
            case specifier: CTypeSpecifier[_] =>
              specifier match {
                case CVoid() | CChar() | CShort() | CInt() | CLong() |
                    CFloat() | CDouble() | CSigned() | CUnsigned() | CBool() |
                    CTypedefName(_) | CFunctionTypeExtensionModifier(_) |
                    CStructDeclaration(_, _) | CStructSpecifier(_) =>
                  Seq(specifier)
              }
            // Const/restrict/volatile/...
            case CTypeQualifierDeclarationSpecifier(_) => Nil
            case _: CFunctionSpecifier[_] => Nil
            case _: CAlignmentSpecifier[_] => Nil
            case _: CGpgpuKernelSpecifier[_] => Nil
          }),
          TypeSize.Unknown(),
        )
    }
  }

  // XXX: We assume that everything's signed unless specified otherwise, this is not actually defined in the spec though
  def isSigned(specs: Seq[CDeclarationSpecifier[_]]): Boolean = {
    specs.collectFirst { case CSpecificationType(t: BitwiseType[_]) =>
      t.signed
    }.getOrElse(
      specs.map {
        case _: CSpecificationModifier[_] => true
        case _: CStorageClassSpecifier[_] => true
        case specifier: CTypeSpecifier[_] =>
          specifier match {
            case CUnsigned() => false
            case CSpecificationType(_) | CVoid() | CChar() |
                CShort() | CInt() | CLong() | CFloat() | CDouble() | CSigned() |
                CBool() | CTypedefName(_) | CFunctionTypeExtensionModifier(_) |
                CStructDeclaration(_, _) | CStructSpecifier(_) =>
              true
          }
        case CTypeQualifierDeclarationSpecifier(_) => true
        case _: CFunctionSpecifier[_] => true
        case _: CAlignmentSpecifier[_] => true
        case _: CGpgpuKernelSpecifier[_] => true
      }.reduce(_ && _)
    )
  }

  case class DeclaratorInfo[G](
      params: Option[Seq[CParam[G]]],
      typeOrReturnType: Type[G] => Type[G],
      name: String,
  )

  def qualify[G](t: Type[G], q: CTypeQualifier[G]): Type[G] = {
    q match {
      case CConst() => TConst(t)(q.o)
      case CUnique(i) => TUnique(t, i)(q.o)
      case pf @ CUniquePointerField(_, i) =>
        val field: CStructMemberDeclarator[G] = pf.ref.get.decls
        val fieldRef: Ref[G, CStructMemberDeclarator[G]] = field.ref
        CTStructUnique(t, fieldRef, i)(q.o)
      case _ => throw CTypeNotSupported(Some(q))
    }
  }

  def processPointer[G](p: CPointer[G], t: Type[G]): Type[G] = {
    p.qualifiers.foldLeft(CTPointer[G](t)(p.o): Type[G])(qualify[G])
  }

  def getDeclaratorInfo[G](decl: CDeclarator[G]): DeclaratorInfo[G] =
    decl match {
      case CPointerDeclarator(pointers, inner) =>
        val innerInfo = getDeclaratorInfo(inner)
        DeclaratorInfo(
          innerInfo.params,
          t =>
            innerInfo.typeOrReturnType(
              pointers.foldLeft(t)((qt, p) => processPointer(p, qt))
            ),
          innerInfo.name,
        )
      case c @ CArrayDeclarator(_, size, inner) =>
        val innerInfo = getDeclaratorInfo(inner)
        DeclaratorInfo(
          innerInfo.params,
          t => CTArray(size, t)(c.blame),
          innerInfo.name,
        )
      case CTypeExtensionDeclarator(Seq(CTypeAttribute(name, Seq(size))), inner)
          if name == "vector_size" || name == "__vector_size__" =>
        val innerInfo = getDeclaratorInfo(inner)
        DeclaratorInfo(
          innerInfo.params,
          t => CTVector(size, innerInfo.typeOrReturnType(t))(decl.o),
          innerInfo.name,
        )
      case typeExtension @ CTypeExtensionDeclarator(_, _) =>
        throw CTypeNotSupported(Some(typeExtension))
      case CTypedFunctionDeclarator(params, _, inner) =>
        val innerInfo = getDeclaratorInfo(inner)
        DeclaratorInfo(
          params = Some(params),
          typeOrReturnType = (t => t),
          innerInfo.name,
        )
      case CAnonymousFunctionDeclarator(Nil, inner) =>
        val innerInfo = getDeclaratorInfo(inner)
        DeclaratorInfo(
          params = Some(Nil),
          typeOrReturnType = (t => t),
          innerInfo.name,
        )
      case decl @ CAnonymousFunctionDeclarator(_, _) =>
        throw AnonymousMethodsUnsupported(decl)
      case CName(name) =>
        DeclaratorInfo(params = None, typeOrReturnType = (t => t), name)
    }

  @tailrec
  private def getSpecs[G](
      decl: CDeclarator[G],
      acc: Seq[CDeclarationSpecifier[G]] = Nil,
  ): Seq[CDeclarationSpecifier[G]] =
    decl match {
      case CTypeExtensionDeclarator(extensions, inner) =>
        getSpecs(inner, acc :+ CFunctionTypeExtensionModifier(extensions))
      case _ => acc
    }

  private def getTypeFromTypeDef[G](
      gdecl: CGlobalDeclaration[G],
      platformContext: Option[PlatformContext],
      context: Option[Node[G]] = None,
  ): Type[G] = {
    val decl = gdecl.decl
    val specs: Seq[CDeclarationSpecifier[G]] =
      decl.specs match {
        case CTypedef() +: remaining => remaining
        case _ => throw CTypeNotSupported(context)
      }

    // Need to get specifications from the init (can only have one init as typedef)
    if (decl.inits.size != 1)
      throw CTypeNotSupported(context)
    val info = getDeclaratorInfo(decl.inits.head.decl)
    if (info.params.isDefined) {
      val returnType = info.typeOrReturnType(
        C.getPrimitiveType(decl.specs, platformContext, context)
      )
      val otherSpecifiers = specs.filter(!_.isInstanceOf[CTypeSpecifier[G]])

      return getPrimitiveType(
        CSpecificationType[G](CTFunction(
          returnType,
          info.params.get.map(p =>
            getDeclaratorInfo(p.declarator).typeOrReturnType(
              getPrimitiveType(p.specifiers, platformContext, context)
            )
          ),
        )) +: otherSpecifiers,
        platformContext,
        context,
      )
    }

    val t =
      specs match {
        case CStructDeclaration(_, _) +: Seq() => CTStruct[G](gdecl.ref)
        case _ => getPrimitiveType(specs, platformContext, context)
      }

    info.typeOrReturnType(t)
  }

  def getPrimitiveType[G](
      specs: Seq[CDeclarationSpecifier[G]],
      platformContext: Option[PlatformContext] = None,
      context: Option[Node[G]] = None,
  ): Type[G] = {
    implicit val o: Origin = context.map(_.o).getOrElse(DiagnosticOrigin)
    val vectorSize: Option[Expr[G]] =
      specs.collect { case ext: CFunctionTypeExtensionModifier[G] =>
        ext.extensions
      } match {
        case Seq(Seq(CTypeAttribute(name, Seq(size: Expr[G]))))
            if name == "__vector_size__" || name == "vector_size" =>
          Some(size)
        case Seq() => None
        case _ => throw CTypeNotSupported(context)
      }

    val (typeSpecs, qualifiers) = specs.filter {
      case _: CTypeSpecifier[G] | _: CTypeQualifierDeclarationSpecifier[G] =>
        true; case _ => false
    }.partition { case _: CTypeSpecifier[G] => true; case _ => false }

    val t: Type[G] =
      typeSpecs match {
        case Seq(CVoid()) => TVoid()
        case t if C.INTEGER_LIKE_TYPES.contains(t) =>
          if (platformContext.isEmpty) { throw UnresolvedCType(specs) }
          val cint = TCInt[G]()
          cint.storedBits = getIntSize(platformContext.get, specs)
          cint.signed = isSigned(specs)
          cint
        case Seq(CFloat()) => C_ieee754_32bit()
        case Seq(CDouble()) => C_ieee754_64bit()
        case Seq(CLong(), CDouble()) => C_ieee754_64bit()
        case Seq(CBool()) => TBool()
        case Seq(defn @ CTypedefName(_)) =>
          defn.ref.get match {
            case RefTypeDef(decl) => getTypeFromTypeDef(decl, platformContext)
            case _ => ???
          }
        case Seq(CSpecificationType(typ)) => typ
        case Seq(defn @ CStructSpecifier(_)) => CTStruct(defn.ref.get.decl.ref)
        case spec +: _ => throw CTypeNotSupported(context.orElse(Some(spec)))
        case _ => throw CTypeNotSupported(context)
      }
    val res =
      vectorSize match {
        case None => t
        case Some(size) => CTVector(size, t)
      }

    qualifiers.collect { case CTypeQualifierDeclarationSpecifier(q) => q }
      .foldLeft(res)(qualify[G])
  }

  def nameFromDeclarator(declarator: CDeclarator[_]): String =
    getDeclaratorInfo(declarator).name

  def typeOrReturnTypeFromDeclaration[G](
      specs: Seq[CDeclarationSpecifier[G]],
      decl: CDeclarator[G],
  ): Type[G] = getDeclaratorInfo(decl).typeOrReturnType(getPrimitiveType(specs))

  def paramsFromDeclarator[G](declarator: CDeclarator[G]): Seq[CParam[G]] =
    getDeclaratorInfo(declarator).params.get

  def findCTypeName[G](
      name: String,
      ctx: TypeResolutionContext[G],
  ): Option[CTypeNameTarget[G]] =
    ctx.stack.flatten.collectFirst {
      case target: CTypeNameTarget[G] if target.name == name => target
    }

  def findCStruct[G](
      name: String,
      ctx: TypeResolutionContext[G],
  ): Option[RefCStruct[G]] =
    ctx.stack.flatten.collectFirst {
      case target: RefCStruct[G] if target.name == name => target
    }

  def findCName[G](
      name: String,
      ctx: ReferenceResolutionContext[G],
  ): Option[CNameTarget[G]] =
    name match {
      case "threadIdx" => Some(RefCudaThreadIdx())
      case "blockDim" => Some(RefCudaBlockDim())
      case "blockIdx" => Some(RefCudaBlockIdx())
      case "gridDim" => Some(RefCudaGridDim())
      case _ =>
        ctx.stack.flatten.collectFirst {
          case target: CNameTarget[G] if target.name == name => target
        }
    }

  def findForwardDeclaration[G](
      declarator: CDeclarator[G],
      ctx: ReferenceResolutionContext[G],
  ): Option[RefCGlobalDeclaration[G]] =
    ctx.stack.flatten.collectFirst {
      case target: RefCGlobalDeclaration[G]
          if target.name == nameFromDeclarator(declarator) =>
        target
    }

  def findDefinition[G](
      declarator: CDeclarator[G],
      ctx: ReferenceResolutionContext[G],
  ): Option[RefCFunctionDefinition[G]] =
    ctx.stack.flatten.collectFirst {
      case target: RefCFunctionDefinition[G]
          if target.name == nameFromDeclarator(declarator) =>
        target
    }

  def stripCPrimitiveType[G](
      t: Type[G],
      platformContext: Option[PlatformContext] = None,
  ): Type[G] =
    t match {
      case CPrimitiveType(specs) => getPrimitiveType(specs, platformContext)
      case _ => t
    }

  def stripUniqueType[G](t: Type[G]): Type[G] =
    t match {
      case CTStructUnique(innerT, _, _) => stripUniqueType(innerT)
      case TUnique(innerT, _) => stripUniqueType(innerT)
      case _ => t
    }

  def findPointerDeref[G](
      obj: Expr[G],
      name: String,
      ctx: ReferenceResolutionContext[G],
      blame: Blame[BuiltinError],
  ): Option[CDerefTarget[G]] =
    stripUniqueType(stripCPrimitiveType(obj.t)) match {
      case CTPointer(t) => findStruct(t, name)
      case CTArray(_, t) => findStruct(t, name)
      case _ => None
    }
//   }

  def findStruct[G](t: Type[G], name: String): Option[CDerefTarget[G]] =
    t match {
      case innerType: TNotAValue[G] =>
        innerType.decl.get match {
          case RefCStruct(decl) => getCStructDeref(decl, name)
          case _ => None
        }
      case struct: CTStruct[G] => getCStructDeref(struct.ref.decl, name)
      case struct: CTStructUnique[G] => findStruct(struct.inner, name)
      case _ => None
    }

  def getCStructDeref[G](
      decl: CGlobalDeclaration[G],
      name: String,
  ): Option[RefCStructField[G]] =
    decl.decl match {
      case CDeclaration(_, _, Seq(CStructDeclaration(_, decls)), Seq()) =>
        decls.flatMap(Referrable.from).collectFirst {
          case ref: RefCStructField[G] if ref.name == name => ref
        }
      case _ => None
    }

  def getUniquePointerStructFieldRef[G](
      specs: Seq[CDeclarationSpecifier[G]],
      pointerField: CUniquePointerField[G],
      ctx: TypeResolutionContext[G],
  ): Option[RefCStructField[G]] = {
    var struct: Option[CStructSpecifier[G]] = None
    specs foreach {
      case s: CStructSpecifier[G] =>
        if (struct.isDefined)
          return None
        struct = Some(s)
      case _ =>
    }
    if (struct.isEmpty)
      return None

    val structRef: RefCStruct[G] = C.findCStruct(struct.get.name, ctx)
      .getOrElse(return None)
    C.getCStructDeref(structRef.decl, pointerField.name)
  }

  def openCLVectorAccessString[G](
      access: String,
      typeSize: BigInt,
  ): Option[Seq[BigInt]] =
    access match {
      case "lo" if typeSize % 2 == 0 =>
        Some(Seq.tabulate(typeSize.toInt / 2)(i => i))
      case "hi" if typeSize % 2 == 0 =>
        Some(Seq.tabulate(typeSize.toInt / 2)(i => i + typeSize / 2))
      case "even" if typeSize % 2 == 0 =>
        Some(Seq.tabulate(typeSize.toInt / 2)(i => 2 * i))
      case "odd" if typeSize % 2 == 0 =>
        Some(Seq.tabulate(typeSize.toInt / 2)(i => 2 * i + 1))
      case s if s.head == 's' && s.tail.nonEmpty =>
        val hexToInt =
          (i: Char) =>
            i match {
              case i if i.isDigit => BigInt(i - '0'.toInt)
              case i if i >= 'a' && i <= 'f' => BigInt(i.toInt - 'a'.toInt + 10)
              case i if i >= 'A' && i <= 'F' => BigInt(i.toInt - 'A'.toInt + 10)
              case _ => return None
            }
        val res = access.tail.map(hexToInt)
        if (res.forall(p => p < typeSize))
          Some(res)
        else
          None
      case _ =>
        val xyzwToInt =
          (i: Char) =>
            i match {
              case 'x' => BigInt(0)
              case 'y' => BigInt(1)
              case 'z' => BigInt(2)
              case 'w' => BigInt(3)
              case _ => return None
            }
        val res = access.map(xyzwToInt)
        if (res.forall(p => p < typeSize))
          Some(res)
        else
          None
    }

  def findDeref[G](
      obj: Expr[G],
      name: String,
      ctx: ReferenceResolutionContext[G],
      blame: Blame[BuiltinError],
  ): Option[CDerefTarget[G]] = {
    (stripUniqueType(stripCPrimitiveType(obj.t)) match {
      case t: TNotAValue[G] =>
        t.decl.get match {
          case RefAxiomaticDataType(decl) =>
            decl.decls.flatMap(Referrable.from).collectFirst {
              case ref: RefADTFunction[G] if ref.name == name => ref
            }
          case RefCStruct(decl: CGlobalDeclaration[G]) =>
            getCStructDeref(decl, name)
          case _ => None
        }
      case struct: CTStruct[G] => getCStructDeref(struct.ref.decl, name)
      case struct: CTStructUnique[G] => findStruct(struct, name)
      case CTCudaVec() =>
        val ref = obj.asInstanceOf[CLocal[G]].ref.get
          .asInstanceOf[RefCudaVec[G]]
        name match {
          case "x" => Some(RefCudaVecX(ref))
          case "y" => Some(RefCudaVecY(ref))
          case "z" => Some(RefCudaVecZ(ref))
          case _ => None
        }
      case v: TOpenCLVector[G] =>
        openCLVectorAccessString(name, v.size).map(RefOpenCLVectorMembers[G])
      case _ => None
    }).orElse(Spec.builtinField(stripCPrimitiveType(obj.t), name, blame, obj.o))
  }

  def resolveInvocation[G](
      obj: Expr[G],
      ctx: ReferenceResolutionContext[G],
  ): CInvocationTarget[G] =
    obj.t match {
      case CTPointer(t @ CTFunction(_, _)) =>
        t.decl match {
          case Some(target) => target
          case _ => throw NotApplicable(obj)
        }
      case t: TNotAValue[G] =>
        t.decl.get match {
          case target: CInvocationTarget[G] => target
          case _ => throw NotApplicable(obj)
        }
      // OpenCL overloads vector literals as function invocations..
      case CPrimitiveType(CSpecificationType(v: TOpenCLVector[G]) +: _) =>
        RefOpenCLVectorLiteralCInvocationTarget[G](v.size, v.innerType)
      case _ => throw NotApplicable(obj)
    }
}
