package vct.col.resolve.lang

import hre.util.FuncTools
import vct.col.ast.`type`.TFloats
import vct.col.ast.{ApplicableContract, Block, CType, EmptyProcess, Expr, JavaAnnotationInterface, JavaClass, JavaClassOrInterface, JavaConstructor, JavaEnum, JavaFields, JavaFinal, JavaImport, JavaInterface, JavaMethod, JavaName, JavaNamedType, JavaNamespace, JavaStatic, JavaTClass, JavaType, JavaVariableDeclaration, LiteralBag, LiteralMap, LiteralSeq, LiteralSet, Null, OptNone, PVLType, TAny, TArray, TAxiomatic, TBag, TBool, TBoundedInt, TChar, TClass, TEither, TEnum, TFloat, TFraction, TInt, TMap, TMatrix, TModel, TNotAValue, TNothing, TNull, TOption, TPointer, TProcess, TRational, TRef, TResource, TSeq, TSet, TString, TTuple, TType, TUnion, TVar, TVoid, TZFraction, Type, UnitAccountedPredicate, Variable, Void}
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.resolve._
import vct.col.resolve.ctx._
import vct.col.typerules.Types
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.{Unreachable, UserError}

import java.lang.reflect.{Modifier, Parameter}
import scala.annotation.tailrec
import scala.collection.mutable

case object Java {
  case class JavaSystemOrigin(preferredName: String) extends Origin {
    override def shortPosition: String = "reflection"
    override def context: String = s"At: [Class loaded from JRE with reflection]"
    override def inlineContext: String = "[Class loaded from JRE with reflection]"
  }

  private implicit val o: Origin = DiagnosticOrigin
  def JAVA_LANG_OBJECT[G]: JavaNamedType[G] = JavaNamedType(Seq(("java", None), ("lang", None), ("Object", None)))
  def JAVA_LANG_ENUM[G]: JavaNamedType[G] = JavaNamedType(Seq(("java", None), ("lang", None), ("Enum", None)))
  def JAVA_LANG_ANNOTATION[G]: JavaNamedType[G] = JavaNamedType(Seq(("java", None), ("lang", None), ("annotation", None), ("Annotation", None)))

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

    println(s"[warning] No specification was found for class ${potentialFQName.mkString(".")}, so a shim will be loaded from the JRE.")

    try {
      val classLoader = this.getClass.getClassLoader
      val cls = classLoader.loadClass(potentialFQName.mkString("."))

      val colClass = translateRuntimeClass(cls)(o, ctx)

      ctx.externallyLoadedElements += new JavaNamespace(Some(JavaName(potentialFQName.init)), Nil, Seq(colClass))

      for(t <- currentlyLoading.remove(potentialFQName).get) {
        ((t : JavaNamedType[_]).unsafeTransmuteGeneration[JavaNamedType, G] : JavaNamedType[G]).ref = Some(RefJavaClass(colClass))
      }

      Some(colClass)
    } catch {
      case _: ClassNotFoundException =>
        currentlyLoading.remove(potentialFQName)
        None
    }
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

  def translateRuntimeParameter[G](param: Parameter)(implicit o: Origin, ctx: TypeResolutionContext[G]): Variable[G] = {
    new Variable(translateRuntimeType(param.getType))(SourceNameOrigin(param.getName, o))
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

    if(cls.isInterface) {
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
              case _ => ???
            }
          case None => None
        }
      case None => None
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

  def findJavaName[G](name: String, ctx: ReferenceResolutionContext[G]): Option[JavaNameTarget[G]] = {
    ctx.stack.flatten.collectFirst {
      case target: JavaNameTarget[G] if target.name == name => target
    }.orElse(ctx.currentJavaNamespace.flatMap(ns => {
      // First find all classes that belong to each import that we can use
      val potentialRefs: Seq[JavaNameTarget[G]] = ns.imports.collect {
        case JavaImport(true, importName, /* star = */ false) if importName.names.last == name =>
          findJavaTypeName(importName.names.init, ctx.asTypeResolutionContext).flatMap {
            case RefJavaClass(cls: JavaClass[G]) => cls.getClassField(name)
            case RefEnum(enum) => enum.getConstant(name)
            case _ => ??? // TODO (RR): ...
          }
        case JavaImport(true, importName, /* star = */ true) =>
          findJavaTypeName(importName.names, ctx.asTypeResolutionContext).flatMap {
            case RefJavaClass(cls: JavaClass[G]) => cls.getClassField(name)
            case RefEnum(enum) => enum.getConstant(name)
            case _ => ??? // TODO (RR): ...
          }
        case JavaImport(false, importName, /* star = */ false) if importName.names.last == name =>
          findJavaTypeName(importName.names, ctx.asTypeResolutionContext).map {
            case r @ RefJavaClass(_) => r
            case r @ RefEnum(_) => r
          }
        case JavaImport(false, importName, /* star = */ true) => // importName.names :+ name
          findJavaTypeName(importName.names :+ name, ctx.asTypeResolutionContext).map {
            case r @ RefJavaClass(cls) => r
            case r @ RefEnum(_) => r
          }
      }.collect { case Some(x) => x }

      potentialRefs match {
        // If we find only one, or none, then that's good
        case Seq(ref) => Some(ref)
        case Nil => None
        // Otherwise there is ambiguity: abort
        // Currently we do not support duplicate imports. E.g. "import static A.X; import static B.*;", given that B
        // would also define a static X, would technically be allowed.
        case _ => throw OverlappingJavaImports(ns, "field", name)
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
            case ref @ RefJavaEnumConstant(constant) if constant.name == name => ref
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
      case ref: RefJavaMethod[G] if ref.name == method && Util.compat(args, ref.decl.parameters) => ref
      case ref: RefJavaAnnotationMethod[G] if ref.name == method && args.length == 0 => ref
      case ref: RefInstanceFunction[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefInstanceMethod[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefInstancePredicate[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
    }

  @tailrec
  final def findMethodOnType[G](t: Type[G], method: String, args: Seq[Expr[G]]): Option[JavaInvocationTarget[G]] =
    t match {
      case TModel(ref) => ref.decl.declarations.flatMap(Referrable.from).collectFirst {
        case ref: RefModelAction[G] if ref.name == method => ref
        case ref: RefModelProcess[G] if ref.name == method => ref
      }
      case JavaTClass(Ref(cls), Nil) => findMethodInClass(cls, method, args)
      case TUnion(ts) => findMethodOnType(Types.leastCommonSuperType(ts), method, args)
      case _ => None
    }

  def findMethod[G](obj: Expr[G], method: String, args: Seq[Expr[G]], blame: Blame[BuiltinError]): Option[JavaInvocationTarget[G]] =
    findMethodOnType(obj.t, method, args).orElse(Spec.builtinInstanceMethod(obj, method, blame))

  def findMethod[G](ctx: ReferenceResolutionContext[G], method: String, args: Seq[Expr[G]]): Option[JavaInvocationTarget[G]] = {
    val selectMatchingSignature: PartialFunction[Referrable[G], JavaInvocationTarget[G]] = {
      case ref: RefJavaMethod[G] if ref.name == method && Util.compat(args, ref.decl.parameters) => ref
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
        case cons: JavaConstructor[G] if Util.compat(args, cons.parameters) => RefJavaConstructor(cons)
      }

      args match {
        case Nil => definedConstructor.orElse(Some(ImplicitDefaultJavaConstructor()))
        case _ => definedConstructor
      }
    case TModel(Ref(model)) if args.isEmpty =>
      Some(RefModel(model))
    case _ => None
  }

  case class WrongDefaultElementArrayType(t: Type[_]) extends UserError {
    override def code: String = "wrongArrElement"
    override def text: String =
      s"It is not possible to initialize an array of which the elements are of type `$t` to default values."
  }

  def zeroValue[G](t: Type[G]): Expr[G] = t match {
    case t: TUnion[G] => throw WrongDefaultElementArrayType(t)
    case t: TVar[G] => throw WrongDefaultElementArrayType(t)
    case TArray(_) => Null()
    case TPointer(_) => Null()
    case TSeq(element) => LiteralSeq(element, Nil)
    case TSet(element) => LiteralSet(element, Nil)
    case TBag(element) => LiteralBag(element, Nil)
    case TOption(_) => OptNone()
    case t: TTuple[G] => throw WrongDefaultElementArrayType(t)
    case t: TEither[G] => throw WrongDefaultElementArrayType(t)
    case t: TMatrix[G] => throw WrongDefaultElementArrayType(t)
    case TMap(key, value) => LiteralMap(key, value, Nil)
    case t: TAny[G] => throw WrongDefaultElementArrayType(t)
    case t: TNothing[G] => throw WrongDefaultElementArrayType(t)
    case TVoid() => Void()
    case TNull() => Null()
    case TBool() => ff
    case t: TResource[G] => throw WrongDefaultElementArrayType(t)
    case t: TChar[G] => throw WrongDefaultElementArrayType(t)
    case TString() => Null()
    case TRef() => Null()
    case TProcess() => EmptyProcess()
    case TInt() => const(0)
    case t: TBoundedInt[G] => throw WrongDefaultElementArrayType(t)
    case t: TFloat[G] => throw WrongDefaultElementArrayType(t)
    case TRational() => const(0)
    case t: TFraction[G] => throw WrongDefaultElementArrayType(t)
    case TZFraction() => const(0)
    case t: TModel[G] => throw WrongDefaultElementArrayType(t)
    case TClass(_) => Null()
    case JavaTClass(_, _) => Null()
    case TEnum(_) => Null()

    case t: TAxiomatic[G] => throw WrongDefaultElementArrayType(t)
    case t: TType[G] => throw WrongDefaultElementArrayType(t)
    case t: CType[G] => throw WrongDefaultElementArrayType(t)
    case t: JavaType[G] => throw WrongDefaultElementArrayType(t)
    case t: PVLType[G] => throw WrongDefaultElementArrayType(t)
    case _: TNotAValue[G] => throw WrongDefaultElementArrayType(t)
  }

  def double[G](implicit o: Origin = DiagnosticOrigin): TFloat[G] = TFloats.ieee754_64bit
  def float[G](implicit o: Origin = DiagnosticOrigin): TFloat[G] = TFloats.ieee754_32bit
}