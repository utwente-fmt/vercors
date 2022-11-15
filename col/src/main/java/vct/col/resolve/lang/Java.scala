package vct.col.resolve.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.FuncTools
import vct.col.ast.lang.JavaAnnotationEx
import vct.col.ast.`type`.TFloats
import vct.col.ast.{ApplicableContract, Block, CType, EmptyProcess, Expr, JavaAnnotationInterface, JavaClass, JavaClassOrInterface, JavaConstructor, JavaFields, JavaFinal, JavaImport, JavaInterface, JavaMethod, JavaName, JavaNamedType, JavaNamespace, JavaStatic, JavaTClass, JavaType, JavaVariableDeclaration, LiteralBag, LiteralMap, LiteralSeq, LiteralSet, Null, OptNone, PVLType, TAny, TAnyClass, TArray, TAxiomatic, TBag, TBool, TBoundedInt, TChar, TClass, TEither, TFloat, TFraction, TInt, TMap, TMatrix, TModel, TNotAValue, TNothing, TNull, TOption, TPointer, TProcess, TRational, TRef, TResource, TSeq, TSet, TString, TTuple, TType, TUnion, TVar, TVoid, TZFraction, Type, UnitAccountedPredicate, Variable, Void, Enum, TPinnedDecl, JavaClassDeclaration, JavaLangString, TEnum, BipPortType, JavaParam, JavaAnnotation, Node}
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.resolve._
import vct.col.resolve.ctx._
import vct.col.typerules.Types
import vct.col.resolve.lang.JavaAnnotationData.{BipComponent, BipData, BipGuard, BipInvariant, BipTransition}
import vct.col.resolve.Resolve.{getLit, isBip}
import vct.result.VerificationError.{Unreachable, UserError}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.{Unreachable, UserError}

import java.lang.reflect.{Modifier, Parameter}
import scala.annotation.tailrec
import scala.collection.mutable

case object Java extends LazyLogging {
  case class UnexpectedJreDefinition(expectedKind: String, fullyQualifiedName: Seq[String]) extends UserError {
    override def text: String = s"Did not get a $expectedKind when resolving $fullyQualifiedName"
    override def code: String = "unexpectedJreDefinition"
  }

  case class JavaSystemOrigin(preferredName: String) extends Origin {
    override def shortPosition: String = "reflection"
    override def context: String = s"At: [Class loaded from JRE with reflection]"
    override def inlineContext: String = "[Class loaded from JRE with reflection]"
  }

  private implicit val o: Origin = DiagnosticOrigin
  def JAVA_LANG_OBJECT[G]: JavaNamedType[G] = JavaNamedType(Seq(("java", None), ("lang", None), ("Object", None)))
  def JAVA_LANG_ANNOTATION_ANNOTATION[G]: JavaNamedType[G] = JavaNamedType(Seq(("java", None), ("lang", None), ("annotation", None), ("Annotation", None)))
  def JAVA_LANG_STRING_TYPE[G]: JavaNamedType[G] = JavaNamedType(Seq(("java", None), ("lang", None), ("String", None)))
  def JAVA_LANG_STRING_NAME[G]: JavaName[G] = JavaName(JAVA_LANG_STRING)
  def JAVA_LANG_CLASS: Seq[String] = Seq("java", "lang", "Class")
  def JAVA_LANG_STRING: Seq[String] = Seq("java", "lang", "String")
  def JAVA_LANG: Seq[String] = Seq("java", "lang")

  def findLoadedJavaTypeName[G](potentialFQName: Seq[String], ctx: TypeResolutionContext[G]): Option[JavaTypeNameTarget[G]] = {
    (ctx.stack.last ++ ctx.externallyLoadedElements.flatMap(Referrable.from)).foreach {
      case RefJavaNamespace(ns: JavaNamespace[G]) =>
        for(decl <- ns.declarations) {
          Referrable.from(decl).foreach {
            case target: JavaTypeNameTarget[G] =>
              if(ns.pkg.map(_.names).getOrElse(Nil) :+ target.name == potentialFQName) {
                return Some(target)
              }
            case _ =>
          }
        }
      case _ =>
    }

    None
  }

  def findLoadedJavaTypeNamesInPackage[G](pkg: Seq[String], ctx: TypeResolutionContext[G]): Seq[JavaTypeNameTarget[G]] =
    (ctx.stack.last ++ ctx.externallyLoadedElements.flatMap(Referrable.from)).collect {
      case RefJavaNamespace(ns: JavaNamespace[G]) if ns.pkg.map(_.names).getOrElse(Nil) == pkg =>
        ns.declarations.flatMap(Referrable.from(_)).collect {
          case target: JavaTypeNameTarget[G] => target
        }
    }.flatten

  private val currentlyLoading = mutable.Map[Seq[String], mutable.ArrayBuffer[JavaNamedType[_ <: Any]]]()

  def lazyType[G](name: Seq[String], ctx: TypeResolutionContext[G]): JavaNamedType[G] = {
    val result = JavaNamedType[G](name.map((_, None)))

    currentlyLoading.get(name) match {
      case Some(lazyQueue) => lazyQueue += result
      case None => result.ref = Some(findJavaTypeName(name, ctx).getOrElse(
        throw Unreachable(s"Invalid JRE: The type `${name.mkString(".")}` built in to the JRE could not be loaded.")))
    }

    result
  }

  def findRuntimeJavaType[G](potentialFQName: Seq[String], ctx: TypeResolutionContext[G]): Option[JavaClassOrInterface[G]] = {
    if(currentlyLoading.contains(potentialFQName))
      throw Unreachable("Aborting cyclic loading of classes from Java runtime")

    implicit val o: Origin = JavaSystemOrigin("unknown_jre")
    currentlyLoading(potentialFQName) = mutable.ArrayBuffer()

    try {
      val classLoader = this.getClass.getClassLoader
      val cls = classLoader.loadClass(potentialFQName.mkString("."))

      val colClass = translateRuntimeClass(cls)(o, ctx)

      ctx.externallyLoadedElements += new JavaNamespace(Some(JavaName(potentialFQName.init)), Nil, Seq(colClass))

      for(t <- currentlyLoading.remove(potentialFQName).get) {
        ((t : JavaNamedType[_]).unsafeTransmuteGeneration[JavaNamedType, G] : JavaNamedType[G]).ref = Some(RefJavaClass(colClass))
      }

      logger.warn(s"No specification was found for class ${potentialFQName.mkString(".")}, so a shim was loaded from the JRE.")

      Some(colClass)
    } catch {
      case _: ClassNotFoundException =>
        currentlyLoading.remove(potentialFQName)
        None
    }
  }

  def findRuntimeJavaTypesInPackage[G](pkg: Seq[String], ctx: TypeResolutionContext[G]): Seq[JavaClassOrInterface[G]] = {
    // TODO...
    Nil
  }

  def translateRuntimeType[G](t: Class[_])(implicit o: Origin, ctx: TypeResolutionContext[G]): Type[G] = t match {
    case java.lang.Boolean.TYPE => TBool()
    case java.lang.Character.TYPE => TChar()
    case java.lang.Byte.TYPE => TInt()
    case java.lang.Short.TYPE => TInt()
    case java.lang.Integer.TYPE => TInt()
    case java.lang.Long.TYPE => TInt()
    case java.lang.Float.TYPE => float
    case java.lang.Double.TYPE => double
    case java.lang.Void.TYPE => TVoid()
    case arr if arr.isArray => TArray(translateRuntimeType(arr.getComponentType))
    case cls => lazyType(cls.getName.split('.').toIndexedSeq, ctx)
  }

  def translateRuntimeParameter[G](param: Parameter)(implicit o: Origin, ctx: TypeResolutionContext[G]): JavaParam[G] = {
    new JavaParam(Seq(), param.getName, translateRuntimeType(param.getType))(SourceNameOrigin(param.getName, o))
  }

  def translateRuntimeClass[G](cls: Class[_])(implicit o: Origin, ctx: TypeResolutionContext[G]): JavaClassOrInterface[G] = {
    val cons = cls.getConstructors.map(cons => {
      new JavaConstructor(
        modifiers = Nil,
        name = cls.getSimpleName,
        parameters = cons.getParameters.toIndexedSeq.map(translateRuntimeParameter[G]),
        typeParameters = Nil,
        signals = Nil,
        body = Block(Nil),
        contract = ApplicableContract(UnitAccountedPredicate(tt), UnitAccountedPredicate(tt), tt, Nil, Nil, Nil, None)(TrueSatisfiable),
      )(SourceNameOrigin(cls.getSimpleName, o))
    })

    val methods = cls.getMethods.map(method => {
      new JavaMethod(
        modifiers = if((method.getModifiers & Modifier.STATIC) != 0) Seq(JavaStatic[G]()) else Nil,
        returnType = translateRuntimeType(method.getReturnType),
        dims = 0,
        name = method.getName,
        parameters = method.getParameters.toIndexedSeq.map(translateRuntimeParameter[G]),
        typeParameters = Nil,
        signals = Nil,
        body = None,
        contract = ApplicableContract(UnitAccountedPredicate(tt), UnitAccountedPredicate(tt), tt, Nil, Nil, Nil, None)(TrueSatisfiable),
      )(AbstractApplicable)(SourceNameOrigin(method.getName, o))
    })

    val fields = cls.getFields.map(field => {
       new JavaFields(
         modifiers =
           (if((field.getModifiers & Modifier.STATIC) != 0) Seq(JavaStatic[G]()) else Nil) ++
           (if((field.getModifiers & Modifier.FINAL) != 0) Seq(JavaFinal[G]()) else Nil),
         t = translateRuntimeType(field.getType),
         decls = Seq(JavaVariableDeclaration[G](field.getName, 0, None)),
       )
    })

    if(cls.isAnnotation) {
      new JavaAnnotationInterface[G](
        name = cls.getName.split('.').last,
        modifiers = Nil,
        ext = cls.getInterfaces.toIndexedSeq.map(cls => lazyType(cls.getName.split('.').toIndexedSeq, ctx))(0),
        decls = fields.toIndexedSeq ++ cons.toIndexedSeq ++ methods.toIndexedSeq,
      )(SourceNameOrigin(cls.getName.split('.').last, o))
    } else if(cls.isInterface) {
      new JavaInterface[G](
        name = cls.getName.split('.').last,
        modifiers = Nil,
        typeParams = Nil,
        ext = cls.getInterfaces.toIndexedSeq.map(cls => lazyType(cls.getName.split('.').toIndexedSeq, ctx)),
        decls = fields.toIndexedSeq ++ cons.toIndexedSeq ++ methods.toIndexedSeq,
      )(SourceNameOrigin(cls.getName.split('.').last, o))
    } else {
      new JavaClass[G](
        name = cls.getName.split('.').last,
        modifiers = Nil,
        typeParams = Nil,
        intrinsicLockInvariant = `tt`,
        ext = Option(cls.getSuperclass).map(cls => lazyType(cls.getName.split('.').toIndexedSeq, ctx)).getOrElse(JAVA_LANG_OBJECT),
        imp = cls.getInterfaces.toIndexedSeq.map(cls => lazyType(cls.getName.split('.').toIndexedSeq, ctx)),
        decls = fields.toIndexedSeq ++ cons.toIndexedSeq ++ methods.toIndexedSeq,
      )(SourceNameOrigin(cls.getName.split('.').last, o))
    }
  }

  def findLibraryJavaType[G](name: Seq[String], ctx: TypeResolutionContext[G]): Option[JavaTypeNameTarget[G]] =
    ctx.externalJavaLoader match {
      case Some(loader) =>
        loader.load[G](name) match {
          case Some(ns) =>
            ctx.externallyLoadedElements += ns
            ResolveTypes.resolve(ns, ctx)
            ns.declarations match {
              case Seq(cls: JavaClass[G]) => Some(RefJavaClass(cls))
              case Seq(cls: JavaInterface[G]) => Some(RefJavaClass(cls))
              case Seq(cls: JavaAnnotationInterface[G]) => Some(RefJavaClass(cls))
              case Seq(enum: Enum[G]) => Some(RefEnum(enum))
              case decls => decls.collect({
                case cls: JavaClassOrInterface[G] if cls.name == name.last => cls
              }) match {
                case Seq(cls: JavaClassOrInterface[G]) => Some(RefJavaClass(cls))
                case _ => None
              }
            }
          case None => None
        }
      case None => None
    }

  def findLibraryJavaTypesInPackage[G](pkg: Seq[String], ctx: TypeResolutionContext[G]): Seq[JavaTypeNameTarget[G]] =
    ctx.externalJavaLoader match {
      case Some(loader) =>
        loader.loadPkg[G](pkg).flatMap { ns =>
          ctx.externallyLoadedElements += ns
          ResolveTypes.resolve(ns, ctx)
          ns.declarations.map {
            case cls: JavaClass[G] => RefJavaClass(cls)
            case cls: JavaInterface[G] => RefJavaClass(cls)
            case cls: JavaAnnotationInterface[G] => RefJavaClass(cls)
            case enum: Enum[G] => RefEnum(enum)
          }
        }
      case None => Seq()
    }

  def findJavaTypeName[G](names: Seq[String], ctx: TypeResolutionContext[G]): Option[JavaTypeNameTarget[G]] = {
    val potentialFQNames: Seq[Seq[String]] = names match {
      case Seq(singleName) =>
        val inThisPackage = ctx.namespace match {
          case Some(ns) => ns.pkg match {
            case Some(pkg) => pkg.names :+ singleName
            case None => Seq(singleName)
          }
          case None => Nil
        }
        val fromImport = ctx.namespace match {
          case Some(ns) => ns.imports.collect {
            case JavaImport(false, name, /*star = */ true) => name.names :+ singleName
            case JavaImport(false, name, /*star = */ false) if name.names.last == singleName => name.names
          }
          case None => Nil
        }
        val fromPredef = Seq("java", "lang", singleName)
        fromImport :+ inThisPackage :+ fromPredef
      case moreNames => Seq(moreNames)
    }

    FuncTools.firstOption(potentialFQNames, findLoadedJavaTypeName[G](_, ctx))
      .orElse(FuncTools.firstOption(potentialFQNames, findLibraryJavaType[G](_, ctx)))
      .orElse(FuncTools.firstOption(potentialFQNames, findRuntimeJavaType[G](_, ctx)).map(RefJavaClass[G]))
  }

  def findJavaTypeNamesInPackage[G](pkg: Seq[String], ctx: TypeResolutionContext[G]): Seq[JavaTypeNameTarget[G]] = {
    (findLoadedJavaTypeNamesInPackage[G](pkg, ctx)
      ++ findLibraryJavaTypesInPackage[G](pkg, ctx)
      ++ findRuntimeJavaTypesInPackage[G](pkg, ctx).map(RefJavaClass[G]))
  }

  def findJavaName[G](name: String, ctx: TypeResolutionContext[G]): Option[JavaNameTarget[G]] = {
    ctx.stack.flatten.collectFirst {
      case target: JavaNameTarget[G] if target.name == name => target
    }.orElse(ctx.namespace.flatMap(ns => {
      def classOrEnum(target: JavaTypeNameTarget[G]): JavaNameTarget[G] = target match {
        case r @ RefJavaClass(_) => r
        case r @ RefEnum(_) => r
      }
      val potentialRefs: Seq[JavaNameTarget[G]] = ns.imports.collect {
        case JavaImport(/* static = */ false, JavaName(fqn), /* star = */ false) if fqn.last == name =>
          findJavaTypeName(fqn, ctx).map(classOrEnum)
        case JavaImport(/* static = */ false, JavaName(pkg), /* star = */ true) =>
          findJavaTypeName(pkg :+ name, ctx).map(classOrEnum)
      }.flatten

      potentialRefs match {
        // If we find only one, or none, then that's good
        case Seq(ref) => Some(ref)
        case Nil => None
        case _ => throw OverlappingJavaImports(ns, "type", name)
      }
    }))
  }

  def findDeref[G](obj: Expr[G], name: String, ctx: ReferenceResolutionContext[G], blame: Blame[BuiltinError]): Option[JavaDerefTarget[G]] =
    ((obj.t match {
      case t: TNotAValue[G] => t.decl.get match {
        case RefUnloadedJavaNamespace(pkg) =>
          Some(findJavaTypeName(pkg :+ name, ctx.asTypeResolutionContext)
            .getOrElse(RefUnloadedJavaNamespace(pkg :+ name)))
        case RefJavaClass(decl) =>
          decl.decls.flatMap(Referrable.from).collectFirst {
            case ref @ RefJavaField(decls, idx) if ref.name == name && ref.decls.modifiers.contains(JavaStatic[G]()) => ref
          }
        case RefEnum(enum) =>
          enum.getConstant(name)
        case _ => None
      }
      case TModel(Ref(model)) => model.declarations.flatMap(Referrable.from).collectFirst {
        case ref @ RefModelField(_) if ref.name == name => ref
      }
      case JavaTClass(Ref(cls), _) => cls.decls.flatMap(Referrable.from).collectFirst {
        case ref @ RefJavaField(_, _) if ref.name == name && !ref.decls.modifiers.contains(JavaStatic[G]()) => ref
      }
      case _ => None
    }) : Option[JavaDerefTarget[G]]).orElse[JavaDerefTarget[G]](Spec.builtinField(obj, name, blame))

  def findMethodInClass[G](cls: JavaClassOrInterface[G], method: String, args: Seq[Expr[G]]): Option[JavaInvocationTarget[G]] =
    cls.decls.flatMap(Referrable.from).collectFirst {
      case ref: RefJavaMethod[G] if ref.name == method && Util.compatJavaParams(args, ref.decl.parameters) => ref
      case ref: RefJavaAnnotationMethod[G] if ref.name == method && args.length == 0 => ref
      case ref: RefInstanceFunction[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefInstanceMethod[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefInstancePredicate[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
    }

  @tailrec
  final def findMethodOnType[G](ctx: ReferenceResolutionContext[G], t: Type[G], method: String, args: Seq[Expr[G]]): Option[JavaInvocationTarget[G]] =
    t match {
      case TModel(ref) => ref.decl.declarations.flatMap(Referrable.from).collectFirst {
        case ref: RefModelAction[G] if ref.name == method => ref
        case ref: RefModelProcess[G] if ref.name == method => ref
      }
      case JavaTClass(Ref(cls), Nil) => findMethodInClass(cls, method, args)
      case TNotAValue(RefJavaClass(cls: JavaClassOrInterface[G])) => findMethodInClass(cls, method, args)
      case TPinnedDecl(JavaLangString(), Nil) =>
        findJavaTypeName[G](Java.JAVA_LANG_STRING, ctx.asTypeResolutionContext).flatMap {
          case cls: RefJavaClass[G] => findMethodInClass[G](cls.decl, method, args)
          case _ => throw UnexpectedJreDefinition("java class", Java.JAVA_LANG_STRING)
        }
      case TUnion(ts) => findMethodOnType(ctx, Types.leastCommonSuperType(ts), method, args)
      case _ => None
    }

  def findMethod[G](ctx: ReferenceResolutionContext[G], obj: Expr[G], method: String, args: Seq[Expr[G]], blame: Blame[BuiltinError]): Option[JavaInvocationTarget[G]] =
    findMethodOnType(ctx, obj.t, method, args).orElse(Spec.builtinInstanceMethod(obj, method, blame))

  def findMethod[G](ctx: ReferenceResolutionContext[G], method: String, args: Seq[Expr[G]]): Option[JavaInvocationTarget[G]] = {
    val selectMatchingSignature: PartialFunction[Referrable[G], JavaInvocationTarget[G]] = {
      case ref: RefJavaMethod[G] if ref.name == method && Util.compatJavaParams(args, ref.decl.parameters) => ref
      case ref: RefInstanceFunction[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefInstanceMethod[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefInstancePredicate[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefFunction[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefProcedure[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefPredicate[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefADTFunction[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefModelProcess[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefModelAction[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
    }

    ctx.stack.flatten.collectFirst(selectMatchingSignature).orElse(ctx.currentJavaNamespace.flatMap(ns => {
      // First find all classes that belong to each import that we can use
      val potentialClasses: Seq[JavaTypeNameTarget[G]] = ns.imports.collect {
        case JavaImport(true, importName, /* star = */ false) if importName.names.last == method => importName.names.init
        case JavaImport(true, importName, /* star = */ true) => importName.names
      }.flatMap(findJavaTypeName(_, ctx.asTypeResolutionContext))

      // From each class, get the method we are looking for
      potentialClasses.collect({
        case RefJavaClass(cls: JavaClass[G]) => cls.getMethods(method)
      }).filter(_.nonEmpty) match { // Discard any classes that have no matches
        // If we find only one set of methods, or no sets at all, then that's good.
        // Just pick the one that matches the signature we're looking for
        case Seq(methods) => methods.collectFirst(selectMatchingSignature)
        case Nil => None
        // If there where multiple classes that had matching definitions, then we error out,
        // because untangling that is not yet supported
        case _ => throw OverlappingJavaImports(ns, "method", method)
      }
    }))
  }

  def findConstructor[G](t: Type[G], args: Seq[Expr[G]]): Option[JavaConstructorTarget[G]] = t match {
    case JavaTClass(Ref(cls), _) =>
      val definedConstructor = cls.decls.collectFirst {
        case cons: JavaConstructor[G] if Util.compatJavaParams(args, cons.parameters) => RefJavaConstructor(cons)
      }

      args match {
        case Nil => definedConstructor.orElse(Some(ImplicitDefaultJavaConstructor()))
        case _ => definedConstructor
      }
    case TModel(Ref(model)) if args.isEmpty =>
      Some(RefModel(model))
    case _ => None
  }


  def findJavaBipStatePredicate[G](ctx: ReferenceResolutionContext[G], state: String): JavaBipStatePredicateTarget[G] = {
    val m = ctx.javaBipStatePredicates.map { case (k, v) => (getLit(k), v) }
    m.get(state) match {
      case Some(ann) => RefJavaBipStatePredicate(ann)
      case None => ImplicitDefaultJavaBipStatePredicate(state)
    }
  }

  def findJavaBipGuard[G](ctx: ReferenceResolutionContext[G], name: String): Option[JavaMethod[G]] =
    ctx.javaBipGuards.map { case (k, v) => (getLit(k), v) }.get(name)

  def getStaticMembers[G](javaTypeName: JavaTypeNameTarget[G]): Seq[Referrable[G]] = javaTypeName match {
    case RefJavaClass(cls) => cls.decls.collect {
      case decl: JavaClassDeclaration[G] if decl.isStatic => Referrable.from(decl)
    }.flatten
    case RefEnum(enum) => enum.constants.map(RefEnumConstant(Some(enum), _))
  }

  def findStaticMember[G](javaTypeName: JavaTypeNameTarget[G], name: String): Option[Referrable[G]] =
    getStaticMembers(javaTypeName).find(_.name == name)

  case class WrongTypeForDefaultValue(t: Type[_]) extends UserError {
    override def code: String = "wrongDefaultType"
    override def text: String =
      t.o.messageInContext(s"The type `$t` has no defined default value in VerCors.")
  }

  def zeroValue[G](t: Type[G]): Expr[G] = t match {
    case t: TUnion[G] => throw WrongTypeForDefaultValue(t)
    case t: TVar[G] => throw WrongTypeForDefaultValue(t)
    case TArray(_) => Null()
    case TPointer(_) => Null()
    case TSeq(element) => LiteralSeq(element, Nil)
    case TSet(element) => LiteralSet(element, Nil)
    case TBag(element) => LiteralBag(element, Nil)
    case TOption(_) => OptNone()
    case t: TTuple[G] => throw WrongTypeForDefaultValue(t)
    case t: TEither[G] => throw WrongTypeForDefaultValue(t)
    case t: TMatrix[G] => throw WrongTypeForDefaultValue(t)
    case TMap(key, value) => LiteralMap(key, value, Nil)
    case t: TAny[G] => throw WrongTypeForDefaultValue(t)
    case t: TNothing[G] => throw WrongTypeForDefaultValue(t)
    case TVoid() => Void()
    case TNull() => Null()
    case TBool() => ff
    case t: TResource[G] => throw WrongTypeForDefaultValue(t)
    case t: TChar[G] => throw WrongTypeForDefaultValue(t)
    case TString() => Null()
    case TRef() => Null()
    case TProcess() => EmptyProcess()
    case TInt() => const(0)
    case t: TBoundedInt[G] => throw WrongTypeForDefaultValue(t)
    case t: TFloat[G] => throw WrongTypeForDefaultValue(t)
    case TRational() => const(0)
    case t: TFraction[G] => throw WrongTypeForDefaultValue(t)
    case TZFraction() => const(0)
    case t: TModel[G] => throw WrongTypeForDefaultValue(t)
    case TClass(_) => Null()
    case JavaTClass(_, _) => Null()
    case TEnum(_) => Null()
    case TAnyClass() => Null()

    case t: TAxiomatic[G] => throw WrongTypeForDefaultValue(t)
    case t: TType[G] => throw WrongTypeForDefaultValue(t)
    case t: CType[G] => throw WrongTypeForDefaultValue(t)
    case t: JavaType[G] => throw WrongTypeForDefaultValue(t)
    case t: PVLType[G] => throw WrongTypeForDefaultValue(t)
    case _: TNotAValue[G] => throw WrongTypeForDefaultValue(t)
  }

  def double[G](implicit o: Origin = DiagnosticOrigin): TFloat[G] = TFloats.ieee754_64bit
  def float[G](implicit o: Origin = DiagnosticOrigin): TFloat[G] = TFloats.ieee754_32bit
}

sealed trait JavaAnnotationData[G]
case object JavaAnnotationData {

  case object BipTransition {
    def get[G](m: JavaMethod[G]): Option[BipTransition[G]] =
      m.modifiers
        .collect { case ja @ JavaAnnotation(_, _) if ja.data.isDefined => ja.data.get }
        .collectFirst { case b: BipTransition[G] => b }
  }
  final case class BipTransition[G](portName: String,
                                    source: JavaBipStatePredicateTarget[G],
                                    target: JavaBipStatePredicateTarget[G],
                                    guard: Option[Expr[G]], requires: Expr[G], ensures: Expr[G]) extends JavaAnnotationData[G]


  case object BipInvariant {
    def get[G](jc: JavaClassOrInterface[G]): Option[BipInvariant[G]] =
      jc.modifiers
        .collect { case ja @ JavaAnnotation(_, _) if ja.data.isDefined => ja.data.get }
        .collectFirst { case bi: BipInvariant[G] => bi }

  }
  final case class BipInvariant[G](expr: Expr[G]) extends JavaAnnotationData[G]

  case object BipPort {
    def getAll[G](c: JavaClass[G]): Seq[BipPort[G]] =
      c.modifiers.collect { case JavaAnnotationEx(_, _, bp: BipPort[G]) => bp}
  }
  final case class BipPort[G](name: String, portType: BipPortType[G])(implicit val o: Origin) extends JavaAnnotationData[G]

  case object BipComponent {
    def get[G](jc: JavaClassOrInterface[G]): Option[BipComponent[G]] =
      jc.modifiers
        .collect { case ja @ JavaAnnotation(_, _) if ja.data.isDefined => ja.data.get }
        .collectFirst { case bct: BipComponent[G] => bct }
  }
  final case class BipComponent[G](name: String, initial: JavaBipStatePredicateTarget[G]) extends JavaAnnotationData[G]

  case object BipStatePredicate {
    def getAll[G](c: JavaClass[G]): Seq[BipStatePredicate[G]] =
      c.modifiers.collect { case JavaAnnotationEx(_, _, bsp: BipStatePredicate[G]) => bsp}
  }
  final case class BipStatePredicate[G](name: String, expr: Expr[G])(implicit val o: Origin) extends JavaAnnotationData[G]

  case object BipData {
    def get[G](node: Node[G]): Option[BipData[G]] = {
      val modifiers = node match {
        case p: JavaParam[G] => p.modifiers
        case m: JavaMethod[G] => m.modifiers
        case _ => return None
      }
      // TODO (RR): Why asInstanceOf here?
      modifiers.collectFirst { case JavaAnnotationEx(_, _, d @ BipData(_)) => d.asInstanceOf[BipData[G]] }
    }
  }
  final case class BipData[G](name: String)(implicit val o: Origin) extends JavaAnnotationData[G]

  case object BipGuard {
    def get[G](m: JavaMethod[G]): Option[BipGuard[G]] =
      m.modifiers
        .collect { case ja @ JavaAnnotation(_, _) if ja.data.isDefined => ja.data.get }
        .collectFirst { case b: BipGuard[G] => b }

    def getName[G](method: JavaMethod[G]): Option[Expr[G]] =
      method.modifiers.collectFirst {
        case ann: JavaAnnotation[G] if isBip(ann, "Guard") => ann.expect("name")
      }
  }
  final case class BipGuard[G](name: String) extends JavaAnnotationData[G]

  case object BipPure {
    def isPure[G](m: JavaMethod[G]): Boolean =
      m.modifiers.collectFirst { case ja @ JavaAnnotationEx(_, _, BipPure()) => ja }.isDefined
  }
  final case class BipPure[G]() extends JavaAnnotationData[G]
}
