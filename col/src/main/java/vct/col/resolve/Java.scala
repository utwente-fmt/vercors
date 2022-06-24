package vct.col.resolve

import hre.util.FuncTools
import vct.col.origin._
import vct.col.ast.{ApplicableContract, Block, Expr, JavaAnnotation, JavaAnnotationInterface, JavaClass, JavaClassOrInterface, JavaConstructor, JavaFields, JavaFinal, JavaImport, JavaInterface, JavaLangString, JavaMethod, JavaName, JavaNamedType, JavaNamespace, JavaStatic, JavaTClass, JavaVariableDeclaration, TArray, TBool, TChar, TFloat, TInt, TModel, TNotAValue, TPinnedDecl, TUnion, TVoid, Type, UnitAccountedPredicate, Variable}
import vct.col.ref.Ref
import vct.col.resolve.Resolve.{getLit, isBip}
import vct.result.VerificationError.{Unreachable, UserError}
import vct.col.util.AstBuildHelpers._
import vct.col.util.Types

import java.lang.reflect.{Modifier, Parameter}
import scala.annotation.tailrec
import scala.collection.mutable

case object Java {
  case class UnexpectedJreDefinition(expectedKind: String, fullyQualifiedName: Seq[String]) extends UserError {
    override def text: String = s"Did not get a $expectedKind when resolving $fullyQualifiedName"
    override def code: String = "unexpectedJreDefinition"
  }

  case class JavaSystemOrigin(preferredName: String) extends Origin {
    override def context: String = s"At: [Class loaded from JRE with reflection]"
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
    case java.lang.Float.TYPE => TFloat()
    case java.lang.Double.TYPE => TFloat()
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
        contract = ApplicableContract(UnitAccountedPredicate(tt), UnitAccountedPredicate(tt), tt, Nil, Nil, Nil),
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
        contract = ApplicableContract(UnitAccountedPredicate(tt), UnitAccountedPredicate(tt), tt, Nil, Nil, Nil),
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
      val potentialClasses: Seq[JavaTypeNameTarget[G]] = ns.imports.collect {
        case JavaImport(true, importName, /* star = */ false) if importName.names.last == name => importName.names.init
        case JavaImport(true, importName, /* star = */ true) => importName.names
      }.flatMap(findJavaTypeName(_, ctx.asTypeResolutionContext))

      // From each class, get the field we are looking for
      potentialClasses.collect({
        case RefJavaClass(cls: JavaClass[G]) => cls.getClassField(name)
      }).flatten match {
        // If we find only one, or none, then that's good
        case Seq(field) => Some(field)
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
          }
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
  final def findMethodOnType[G](ctx: ReferenceResolutionContext[G], t: Type[G], method: String, args: Seq[Expr[G]]): Option[JavaInvocationTarget[G]] =
    t match {
      case TModel(ref) => ref.decl.declarations.flatMap(Referrable.from).collectFirst {
        case ref: RefModelAction[G] if ref.name == method => ref
        case ref: RefModelProcess[G] if ref.name == method => ref
      }
      case JavaTClass(Ref(cls), Nil) => findMethodInClass(cls, method, args)
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

  def findJavaBipStatePredicate[G](ctx: ReferenceResolutionContext[G], state: String): JavaBipStatePredicateTarget[G] =
     ctx.javaBipStatePredicates.get(state) match {
      case Some(ann) => RefJavaBipStatePredicate(ann)
      case None => ImplicitDefaultJavaBipStatePredicate(state)
    }

  def getJavaBipGuardName[G](method: JavaMethod[G]): Option[String] =
    method.modifiers.collectFirst {
      case ann: JavaAnnotation[G] if isBip(ann, "Guard") => getLit(ann.expect("name"))
    }
}