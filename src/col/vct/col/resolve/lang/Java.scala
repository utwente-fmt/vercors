package vct.col.resolve.lang

import com.typesafe.scalalogging.LazyLogging
import hre.util.FuncTools
import vct.col.ast.`type`.typeclass.TFloats
import vct.col.ast.lang.java.JavaAnnotationEx
import vct.col.ast.{
  ApplicableContract,
  AxiomaticDataType,
  BipPortType,
  Block,
  EmptyProcess,
  Expr,
  JavaAnnotation,
  JavaAnnotationInterface,
  JavaClass,
  JavaClassDeclaration,
  JavaClassOrInterface,
  JavaConstructor,
  JavaFields,
  JavaFinal,
  JavaImport,
  JavaInterface,
  JavaMethod,
  JavaModifier,
  JavaName,
  JavaNamedType,
  JavaNamespace,
  JavaParam,
  JavaStatic,
  JavaTClass,
  JavaVariableDeclaration,
  JavaWildcard,
  LiteralBag,
  LiteralMap,
  LiteralSeq,
  LiteralSet,
  Node,
  Null,
  OptNone,
  TAnyClass,
  TArray,
  TBag,
  TBool,
  TByReferenceClass,
  TChar,
  TClass,
  TEnum,
  TFloat,
  TInt,
  TMap,
  TModel,
  TNotAValue,
  TNull,
  TOption,
  TPointer,
  TProcess,
  TRational,
  TRef,
  TSeq,
  TSet,
  TString,
  TUnion,
  TVoid,
  TZFraction,
  Type,
  UnitAccountedPredicate,
  Variable,
  Void,
}
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.resolve.Resolve.{getLit, isBip}
import vct.col.resolve.ResolveTypes.JavaClassPathEntry
import vct.col.resolve._
import vct.col.resolve.ctx._
import vct.col.typerules.Types
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.{Unreachable, UserError}

import java.lang.reflect
import java.lang.reflect.{Modifier, Parameter, TypeVariable}
import java.nio.file.Path
import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

case object Java extends LazyLogging {
  case class UnexpectedJreDefinition(
      expectedKind: String,
      fullyQualifiedName: Seq[String],
  ) extends UserError {
    override def text: String =
      s"Did not get a $expectedKind when resolving $fullyQualifiedName"
    override def code: String = "unexpectedJreDefinition"
  }

  object JRESource extends Source {
    override def positionContext(position: PositionRange): String =
      genericContext
    override def inlinePositionContext(
        position: PositionRange,
        compress: Boolean,
    ): String = genericInlineContext
    override protected def genericContext: String =
      "At node loaded reflectively from the JRE"
    override protected def genericInlineContext: String = "JRE"
    override def genericShortPosition: String = "JRE"
  }

  private def JavaSystemOrigin(): Origin = Origin(Seq(JRESource))

  private implicit val o: Origin = DiagnosticOrigin
  def JAVA_LANG_OBJECT[G]: JavaNamedType[G] =
    JavaNamedType(Seq(("java", None), ("lang", None), ("Object", None)))
  def JAVA_LANG_ANNOTATION_ANNOTATION[G]: JavaNamedType[G] =
    JavaNamedType(Seq(
      ("java", None),
      ("lang", None),
      ("annotation", None),
      ("Annotation", None),
    ))
  def JAVA_LANG_STRING_TYPE[G]: JavaNamedType[G] =
    JavaNamedType(Seq(("java", None), ("lang", None), ("String", None)))
  def JAVA_LANG_STRING_NAME[G]: JavaName[G] = JavaName(JAVA_LANG_STRING)
  def JAVA_LANG_CLASS: Seq[String] = JAVA_LANG :+ "Class"
  def JAVA_LANG_STRING: Seq[String] = JAVA_LANG :+ "String"
  def JAVA_LANG: Seq[String] = Seq("java", "lang")

  implicit class ModifiersHelpers[G](modifiers: Seq[JavaModifier[G]]) {
    def all[T <: JavaModifier[G]](implicit tag: ClassTag[T]): Seq[T] =
      modifiers.collect { case t: T => t }
    def first[T <: JavaModifier[G]](implicit tag: ClassTag[T]): Option[T] =
      modifiers.collectFirst { case t: T => t }
    def is[T <: JavaModifier[G]](implicit tag: ClassTag[T]): Boolean =
      first[T].nonEmpty
    def isNot[T <: JavaModifier[G]](implicit tag: ClassTag[T]): Boolean =
      first[T].isEmpty
  }

  def findLoadedJavaTypeName[G](
      potentialFQName: Seq[String],
      ctx: TypeResolutionContext[G],
  ): Option[JavaTypeNameTarget[G]] = {
    (ctx.stack.lastOption.getOrElse(Seq()) ++
      ctx.externallyLoadedElements.flatMap(Referrable.from)).foreach {
      case RefJavaNamespace(ns: JavaNamespace[G]) =>
        for (decl <- ns.declarations) {
          Referrable.from(decl).foreach {
            case target: JavaTypeNameTarget[G] =>
              if (
                ns.pkg.map(_.names).getOrElse(Nil) :+ target.name ==
                  potentialFQName
              ) { return Some(target) }
            case _ =>
          }
        }
      case _ =>
    }

    None
  }

  def findLoadedJavaTypeNamesInPackage[G](
      pkg: Seq[String],
      ctx: TypeResolutionContext[G],
  ): Seq[JavaTypeNameTarget[G]] =
    (ctx.stack.last ++ ctx.externallyLoadedElements.flatMap(Referrable.from))
      .collect {
        case RefJavaNamespace(ns: JavaNamespace[G])
            if ns.pkg.map(_.names).getOrElse(Nil) == pkg =>
          ns.declarations.flatMap(Referrable.from(_)).collect {
            case target: JavaTypeNameTarget[G] => target
          }
      }.flatten

  private val currentlyLoading = mutable
    .Map[Seq[String], mutable.ArrayBuffer[JavaNamedType[_ <: Any]]]()

  def lazyType[G](t: reflect.Type, ctx: TypeResolutionContext[G]): Type[G] =
    t match {
      case t: reflect.ParameterizedType =>
        // Blatantly assume generic types are always classes applied to some type arguments
        val name = t.getRawType.asInstanceOf[Class[_]].getName
        // TODO: DiagnosticOrigin makes no sense here
        lazyType(
          name,
          t.getActualTypeArguments
            .map(translateRuntimeType(_)(DiagnosticOrigin, ctx)).toIndexedSeq,
          ctx,
        )
      case t: java.lang.Class[_] =>
        val name = t.getName
        lazyType(name, Seq(), ctx)
      case _ =>
        // Don't call this method with primitive or array types
        // These are handled in translateRuntimeType
        throw Unreachable(???)
    }

  def lazyType[G](
      name: String,
      typeArgs: Seq[Type[G]],
      ctx: TypeResolutionContext[G],
  ): Type[G] = lazyType(name.split('.').toIndexedSeq, typeArgs, ctx)

  def lazyType[G](
      name: Seq[String],
      typeArgs: Seq[Type[G]],
      ctx: TypeResolutionContext[G],
  ): Type[G] = {
    val result = JavaNamedType[G](
      name.init.map((_, None)) :+ (name.last, Some(typeArgs))
    )

    currentlyLoading.get(name) match {
      case Some(lazyQueue) => lazyQueue += result
      case None =>
        result.ref = Some(findJavaTypeName(name, ctx).getOrElse(
          throw Unreachable(
            s"Invalid JRE: The type `${name.mkString(".")}` built in to the JRE could not be loaded."
          )
        ))
    }

    result
  }

  def findRuntimeJavaType[G](
      potentialFQName: Seq[String],
      ctx: TypeResolutionContext[G],
  ): Option[JavaClassOrInterface[G]] = {
    if (currentlyLoading.contains(potentialFQName))
      throw Unreachable("Aborting cyclic loading of classes from Java runtime")

    implicit val o: Origin = JavaSystemOrigin()
    currentlyLoading(potentialFQName) = mutable.ArrayBuffer()

    logger.warn(
      s"Attempting to load a shim of ${potentialFQName.mkString(".")} via reflection."
    )

    try {
      val classLoader = this.getClass.getClassLoader
      val cls = classLoader.loadClass(potentialFQName.mkString("."))

      val colClass = translateRuntimeClass(cls)(o, ctx)

      ctx.externallyLoadedElements += new JavaNamespace(
        Some(JavaName(potentialFQName.init)),
        Nil,
        Seq(colClass),
      )

      for (t <- currentlyLoading.remove(potentialFQName).get) {
        ((t: JavaNamedType[_])
          .unsafeTransmuteGeneration[JavaNamedType, G]: JavaNamedType[G]).ref =
          Some(RefJavaClass(colClass))
      }

      logger.warn(
        s"No specification was found for class ${potentialFQName.mkString(".")}, so a shim was loaded from the JRE."
      )

      Some(colClass)
    } catch {
      case _: ClassNotFoundException =>
        currentlyLoading.remove(potentialFQName)
        None
    }
  }

  def findRuntimeJavaTypesInPackage[G](
      pkg: Seq[String],
      ctx: TypeResolutionContext[G],
  ): Seq[JavaClassOrInterface[G]] = {
    // TODO...
    Nil
  }

  def translateRuntimeType[G](
      t: reflect.Type
  )(implicit o: Origin, ctx: TypeResolutionContext[G]): Type[G] =
    t match {
      case java.lang.Boolean.TYPE => TBool()
      case java.lang.Character.TYPE => TChar()
      case java.lang.Byte.TYPE => TInt()
      case java.lang.Short.TYPE => TInt()
      case java.lang.Integer.TYPE => TInt()
      case java.lang.Long.TYPE => TInt()
      case java.lang.Float.TYPE => float
      case java.lang.Double.TYPE => double
      case java.lang.Void.TYPE => TVoid()
      case arr: reflect.GenericArrayType =>
        TArray(translateRuntimeType(arr.getGenericComponentType))
      case t: reflect.WildcardType => JavaWildcard()
      case t: reflect.Type => lazyType(t, ctx)
    }

  def translateRuntimeParameter[G](
      param: Parameter,
      t: reflect.Type,
  )(implicit o: Origin, ctx: TypeResolutionContext[G]): JavaParam[G] = {
    new JavaParam(Seq(), param.getName, translateRuntimeType(t))(
      o.where(name = param.getName)
    )
  }

  def translateTypeParameter[G](
      param: TypeVariable[_]
  )(implicit o: Origin, ctx: TypeResolutionContext[G]): Variable[G] = {
    // Guess that bound 0 is the one we want
    val bound =
      param.getBounds()(0) match {
        case cls: Class[_] =>
          JavaTClass(
            translateRuntimeClass(cls).ref[JavaClassOrInterface[G]],
            Seq(),
          ) // Pretty sure the seq is not always empty here
        case typeVar: TypeVariable[_] =>
          ??? // We need some extra state here to find where this type var comes from
      }
    new Variable(bound)(o.where(name = param.getName))
  }

  def translateRuntimeClass[G](cls: Class[_])(
      implicit o: Origin,
      ctx: TypeResolutionContext[G],
  ): JavaClassOrInterface[G] = {
    val cons = cls.getConstructors.map(cons => {
      new JavaConstructor(
        modifiers = Nil,
        name = cls.getSimpleName,
        parameters = {
          val params = cons.getParameters.toIndexedSeq
          val types = cons.getGenericParameterTypes.toIndexedSeq
          params.zip(types).map { case (p, t) =>
            translateRuntimeParameter[G](p, t)
          }
        },
        typeParameters = Nil,
        signals = Nil,
        body = Block(Nil),
        contract =
          ApplicableContract(
            UnitAccountedPredicate(tt),
            UnitAccountedPredicate(tt),
            tt,
            Nil,
            Nil,
            Nil,
            None,
          )(TrueSatisfiable),
      )(o.where(name = cls.getSimpleName))
    })

    val methods = cls.getMethods.map(method => {
      new JavaMethod(
        modifiers =
          if ((method.getModifiers & Modifier.STATIC) != 0)
            Seq(JavaStatic[G]())
          else
            Nil,
        returnType = translateRuntimeType(method.getGenericReturnType),
        dims = 0,
        name = method.getName,
        parameters = {
          val params = method.getParameters.toIndexedSeq
          val types = method.getGenericParameterTypes.toIndexedSeq
          params.zip(types).map { case (p, t) =>
            translateRuntimeParameter[G](p, t)
          }
        },
        typeParameters = Nil,
        signals = Nil,
        body = None,
        contract =
          ApplicableContract(
            UnitAccountedPredicate(tt),
            UnitAccountedPredicate(tt),
            tt,
            Nil,
            Nil,
            Nil,
            None,
          )(TrueSatisfiable),
      )(AbstractApplicable)(o.where(name = method.getName))
    })

    val fields = cls.getFields.map(field => {
      new JavaFields(
        modifiers =
          (if ((field.getModifiers & Modifier.STATIC) != 0)
             Seq(JavaStatic[G]())
           else
             Nil) ++
            (if ((field.getModifiers & Modifier.FINAL) != 0)
               Seq(JavaFinal[G]())
             else
               Nil),
        t = translateRuntimeType(field.getType),
        decls = Seq(JavaVariableDeclaration[G](field.getName, 0, None)),
      )
    })

    val typeParameters = cls.getTypeParameters
      .map(translateTypeParameter(_)(o, ctx))

    if (cls.isAnnotation) {
      new JavaAnnotationInterface[G](
        name = cls.getName.split('.').last,
        modifiers = Nil,
        ext =
          cls.getGenericInterfaces.toIndexedSeq
            .map(cls => lazyType(cls, ctx))(0),
        decls = fields.toIndexedSeq ++ cons.toIndexedSeq ++ methods.toIndexedSeq,
      )(o.where(name = cls.getName.split('.').last))
    } else if (cls.isInterface) {
      new JavaInterface[G](
        name = cls.getName.split('.').last,
        modifiers = Nil,
        typeParams = typeParameters,
        ext = cls.getGenericInterfaces.toIndexedSeq
          .map(cls => lazyType(cls, ctx)),
        decls = fields.toIndexedSeq ++ cons.toIndexedSeq ++ methods.toIndexedSeq,
      )(o.where(name = cls.getName.split('.').last))
    } else {
      new JavaClass[G](
        name = cls.getName.split('.').last,
        modifiers = Nil,
        typeParams = typeParameters,
        intrinsicLockInvariant = `tt`,
        ext = Option(cls.getGenericSuperclass).map(cls => lazyType(cls, ctx))
          .getOrElse(JAVA_LANG_OBJECT),
        imp = cls.getGenericInterfaces.toIndexedSeq
          .map(cls => lazyType(cls, ctx)),
        decls = fields.toIndexedSeq ++ cons.toIndexedSeq ++ methods.toIndexedSeq,
      )(o.where(name = cls.getName.split('.').last))
    }
  }

  /** Attempt to find a class by its fully qualified name.
    * @param name
    *   Fully qualified name of the class
    * @param ctx
    *   Queried for the external class loader, and the source class paths
    */
  def findLibraryJavaType[G](
      name: Seq[String],
      ctx: TypeResolutionContext[G],
  ): Option[JavaTypeNameTarget[G]] =
    ctx.externalJavaLoader.flatMap { loader =>
      FuncTools.firstOption(
        ctx.javaClassPath,
        (classPath: ResolveTypes.JavaClassPathEntry) => {
          // We have an external class loader and a class path.
          val maybeBasePath: Option[Path] =
            classPath match {
              case JavaClassPathEntry.SourcePackageRoot =>
                // Try to derive the base path, by the path of the current source file and the package.
                // E.g. /root/pkg/a/Cls.java declaring package pkg.a; -> /root
                for {
                  ns <- ctx.namespace
                  readableOrigin <- ns.o.find[ReadableOrigin]
                  readable <- Some(readableOrigin.readable)
                  file <- readable.underlyingPath
                  baseFile <-
                    ns.pkg.getOrElse(JavaName(Nil)).names
                      .foldRight[Option[Path]](Option(file.getParent)) {
                        case (name, Some(file))
                            if file.getFileName.toString == name =>
                          Option(file.getParent)
                        case _ => None
                      }
                } yield baseFile
              case JavaClassPathEntry.Path(root) => Some(root)
            }

          for {
            basePath <- maybeBasePath
            ns <- loader.load[G](basePath, name)
            cls <- ns.declarations.flatMap(Referrable.from).collectFirst {
              case ref: JavaTypeNameTarget[G] if ref.name == name.last =>
                ctx.externallyLoadedElements += ns
                ResolveTypes.resolve(ns, ctx)
                ref
            }
          } yield cls
        },
      )
    }

  def findJavaTypeName[G](
      names: Seq[String],
      ctx: TypeResolutionContext[G],
  ): Option[JavaTypeNameTarget[G]] = {
    val potentialFQNames: Seq[Seq[String]] =
      names match {
        case Seq(singleName) =>
          val inThisPackage =
            ctx.namespace match {
              case Some(ns) =>
                ns.pkg match {
                  case Some(pkg) => pkg.names :+ singleName
                  case None => Seq(singleName)
                }
              case None => Nil
            }
          val fromImport =
            ctx.namespace match {
              case Some(ns) =>
                ns.imports.collect {
                  case JavaImport(false, name, /*star = */ true) =>
                    name.names :+ singleName
                  case JavaImport(false, name, /*star = */ false)
                      if name.names.last == singleName =>
                    name.names
                }
              case None => Nil
            }
          val fromPredef = Seq("java", "lang", singleName)
          fromImport :+ inThisPackage :+ fromPredef
        case moreNames => Seq(moreNames)
      }

    FuncTools.firstOption(potentialFQNames, findJavaTypeInStack[G](_, ctx))
      .orElse(FuncTools.firstOption(
        potentialFQNames,
        findLoadedJavaTypeName[G](_, ctx),
      )).orElse(FuncTools.firstOption(
        potentialFQNames,
        findLibraryJavaType[G](_, ctx),
      )).orElse(
        FuncTools.firstOption(potentialFQNames, findRuntimeJavaType[G](_, ctx))
          .map(RefJavaClass[G])
      )
  }

  def findJavaTypeInStack[G](
      name: Seq[String],
      ctx: TypeResolutionContext[G],
  ): Option[JavaTypeNameTarget[G]] = {
    ctx.stack.flatten.collectFirst {
      case ref: JavaTypeNameTarget[G] if Seq(ref.name) == name => ref
    }
  }

  def allowedFromStaticContext[G](target: JavaNameTarget[G]): Boolean =
    target match {
      case RefAxiomaticDataType(_) => true
      case RefClass(_) => true // PB: except maybe in non-static inner classes?
      case RefEnum(_) => true
      case RefEnumConstant(_, _) => true
      case RefJavaClass(decl) => true
      case RefUnloadedJavaNamespace(names) => true

      case RefJavaField(decls, _) => decls.modifiers.is[JavaStatic[G]]
      case RefModelField(_) => false
      case RefJavaBipGuard(_) => false

      case RefVariable(_) => true
      case RefJavaLocalDeclaration(_, _) => true
      case RefJavaParam(_) => true
    }

  def findJavaName[G](
      name: String,
      fromStaticContext: Boolean,
      ctx: TypeResolutionContext[G],
  ): Option[JavaNameTarget[G]] = {
    ctx.stack.flatten.collectFirst {
      case target: JavaNameTarget[G]
          if target.name == name &&
            (!fromStaticContext || allowedFromStaticContext(target)) =>
        target
    }.orElse(ctx.namespace.flatMap(ns => {
      // PB: I optionified this, but not entirely sure what the intention is here.
      def classOrEnum(
          target: JavaTypeNameTarget[G]
      ): Option[JavaNameTarget[G]] =
        target match {
          case r @ RefJavaClass(_) => Some(r)
          case r @ RefEnum(_) => Some(r)
          case _ => None
        }
      val potentialRefs: Seq[JavaNameTarget[G]] =
        ns.imports.collect {
          case JavaImport(
                /* static = */ false,
                JavaName(fqn), /* star = */ false,
              ) if fqn.last == name =>
            findJavaTypeName(fqn, ctx).flatMap(classOrEnum)
          case JavaImport(
                /* static = */ false,
                JavaName(pkg), /* star = */ true,
              ) =>
            findJavaTypeName(pkg :+ name, ctx).flatMap(classOrEnum)
        }.flatten

      potentialRefs match {
        // If we find only one, or none, then that's good
        case Seq(ref) => Some(ref)
        case Nil => None
        case _ => throw OverlappingJavaImports(ns, "type", name)
      }
    }))
  }

  def findDeref[G](
      obj: Expr[G],
      name: String,
      ctx: ReferenceResolutionContext[G],
      blame: Blame[BuiltinError],
  ): Option[JavaDerefTarget[G]] =
    ((obj.t match {
      case t: TNotAValue[G] =>
        t.decl.get match {
          case RefUnloadedJavaNamespace(pkg) =>
            Some(
              findJavaTypeName(pkg :+ name, ctx.asTypeResolutionContext)
                .getOrElse(RefUnloadedJavaNamespace(pkg :+ name))
            )
          case RefJavaClass(decl) =>
            decl.decls.flatMap(Referrable.from).collectFirst {
              case ref @ RefJavaField(decls, idx)
                  if ref.name == name &&
                    ref.decls.modifiers.is[JavaStatic[G]] =>
                ref
            }
          case RefEnum(enum) => enum.getConstant(name)
          case _ => None
        }
      case TModel(Ref(model)) =>
        model.declarations.flatMap(Referrable.from).collectFirst {
          case ref @ RefModelField(_) if ref.name == name => ref
        }
      case JavaTClass(Ref(cls), _) =>
        cls.decls.flatMap(Referrable.from).collectFirst {
          case ref @ RefJavaField(_, _)
              if ref.name == name && !ref.decls.modifiers.is[JavaStatic[G]] =>
            ref
        }
      case _ => None
    }): Option[JavaDerefTarget[G]])
      .orElse[JavaDerefTarget[G]](Spec.builtinField(obj, name, blame))

  def findMethodInClass[G](
      cls: JavaClassOrInterface[G],
      fromStaticContext: Boolean,
      method: String,
      args: Seq[Expr[G]],
  ): Option[JavaInvocationTarget[G]] =
    cls.decls.flatMap(Referrable.from).collectFirst {
      case ref: RefJavaMethod[G]
          if ref.name == method &&
            Util.compatJavaParams(args, ref.decl.parameters) &&
            (!fromStaticContext || ref.decl.modifiers.is[JavaStatic[G]]) =>
        ref
      case ref: RefJavaAnnotationMethod[G]
          if ref.name == method && args.isEmpty && !fromStaticContext =>
        ref
      case ref: RefInstanceFunction[G]
          if ref.name == method && Util.compat(args, ref.decl.args) &&
            !fromStaticContext =>
        ref
      case ref: RefInstanceMethod[G]
          if ref.name == method && Util.compat(args, ref.decl.args) &&
            !fromStaticContext =>
        ref
      case ref: RefInstancePredicate[G]
          if ref.name == method && Util.compat(args, ref.decl.args) &&
            !fromStaticContext =>
        ref
    }

  @tailrec
  final def findMethodOnType[G](
      ctx: ReferenceResolutionContext[G],
      t: Type[G],
      method: String,
      args: Seq[Expr[G]],
  ): Option[JavaInvocationTarget[G]] =
    t match {
      case TModel(ref) =>
        ref.decl.declarations.flatMap(Referrable.from).collectFirst {
          case ref: RefModelAction[G] if ref.name == method => ref
          case ref: RefModelProcess[G] if ref.name == method => ref
        }
      case JavaTClass(Ref(cls), Nil) =>
        findMethodInClass(cls, fromStaticContext = false, method, args)
      case TUnion(ts) =>
        findMethodOnType(ctx, Types.leastCommonSuperType(ts), method, args)
      case TNotAValue(RefJavaClass(cls: JavaClassOrInterface[G @unchecked])) =>
        findMethodInClass[G](cls, fromStaticContext = true, method, args)
      case TNotAValue(
            RefAxiomaticDataType(adt: AxiomaticDataType[G @unchecked])
          ) =>
        adt.decls.flatMap(Referrable.from).collectFirst {
          case ref: RefADTFunction[G]
              if ref.name == method && Util.compat(args, ref.decl.args) =>
            ref
        }
      case _ => None
    }

  def findMethod[G](
      ctx: ReferenceResolutionContext[G],
      obj: Expr[G],
      method: String,
      args: Seq[Expr[G]],
      blame: Blame[BuiltinError],
  ): Option[JavaInvocationTarget[G]] =
    findMethodOnType(ctx, obj.t, method, args)
      .orElse(Spec.builtinInstanceMethod(obj, method, blame))

  def matchesStatic(
      ctx: ReferenceResolutionContext[_],
      modifiers: Seq[JavaModifier[_]],
  ): Boolean =
    !ctx.inStaticJavaContext || modifiers.collectFirst {
      case _: JavaStatic[_] => ()
    }.nonEmpty

  def findMethod[G](
      ctx: ReferenceResolutionContext[G],
      method: String,
      args: Seq[Expr[G]],
  ): Option[JavaInvocationTarget[G]] = {
    val selectMatchingSignature
        : PartialFunction[Referrable[G], JavaInvocationTarget[G]] = {
      case ref: RefJavaMethod[G]
          if ref.name == method &&
            Util.compatJavaParams(args, ref.decl.parameters) &&
            matchesStatic(ctx, ref.decl.modifiers) =>
        ref
      case ref: RefInstanceFunction[G]
          if ref.name == method && Util.compat(args, ref.decl.args) &&
            !ctx.inStaticJavaContext =>
        ref
      case ref: RefInstanceMethod[G]
          if ref.name == method && Util.compat(args, ref.decl.args) &&
            !ctx.inStaticJavaContext =>
        ref
      case ref: RefInstancePredicate[G]
          if ref.name == method && Util.compat(args, ref.decl.args) &&
            !ctx.inStaticJavaContext =>
        ref
      case ref: RefFunction[G]
          if ref.name == method && Util.compat(args, ref.decl.args) =>
        ref
      case ref: RefProcedure[G]
          if ref.name == method && Util.compat(args, ref.decl.args) =>
        ref
      case ref: RefPredicate[G]
          if ref.name == method && Util.compat(args, ref.decl.args) =>
        ref
      case ref: RefADTFunction[G]
          if ref.name == method && Util.compat(args, ref.decl.args) =>
        ref
      case ref: RefModelProcess[G]
          if ref.name == method && Util.compat(args, ref.decl.args) =>
        ref
      case ref: RefModelAction[G]
          if ref.name == method && Util.compat(args, ref.decl.args) =>
        ref
      case ref: RefProverFunction[G]
          if ref.name == method && Util.compat(args, ref.decl.args) =>
        ref
    }

    ctx.stack.flatten.collectFirst(selectMatchingSignature)
      .orElse(ctx.currentJavaNamespace.flatMap(ns => {
        // First find all classes that belong to each import that we can use
        val potentialClasses: Seq[JavaTypeNameTarget[G]] = ns.imports.collect {
          case JavaImport(true, importName, /* star = */ false)
              if importName.names.last == method =>
            importName.names.init
          case JavaImport(true, importName, /* star = */ true) =>
            importName.names
        }.flatMap(findJavaTypeName(_, ctx.asTypeResolutionContext))

        // From each class, get the method we are looking for
        potentialClasses.collect({ case RefJavaClass(cls: JavaClass[G]) =>
          cls.getMethods(method)
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

  def findConstructor[G](
      t: Type[G],
      args: Seq[Expr[G]],
  ): Option[JavaConstructorTarget[G]] =
    t match {
      case JavaTClass(Ref(cls: JavaClass[G]), _) =>
        val definedConstructor = cls.decls.collectFirst {
          case cons: JavaConstructor[G]
              if Util.compatJavaParams(args, cons.parameters) =>
            RefJavaConstructor(cons)
        }

        args match {
          case Nil =>
            definedConstructor.orElse(Some(ImplicitDefaultJavaConstructor(cls)))
          case _ => definedConstructor
        }
      case TModel(Ref(model)) if args.isEmpty => Some(RefModel(model))
      case _ => None
    }

  def findJavaBipStatePredicate[G](
      ctx: ReferenceResolutionContext[G],
      state: String,
  ): JavaBipStatePredicateTarget[G] = {
    val m = ctx.javaBipStatePredicates.map { case (k, v) => (getLit(k), v) }
    m.get(state) match {
      case Some(ann) => RefJavaBipStatePredicate(state, ann)
      case None => ImplicitDefaultJavaBipStatePredicate(state)
    }
  }

  def findJavaBipGuard[G](
      ctx: ReferenceResolutionContext[G],
      name: String,
  ): Option[JavaMethod[G]] =
    ctx.javaBipGuards.map { case (k, v) => (getLit(k), v) }.get(name)

  def getStaticMembers[G](
      javaTypeName: JavaTypeNameTarget[G]
  ): Seq[Referrable[G]] =
    javaTypeName match {
      case RefJavaClass(cls) =>
        cls.decls.collect {
          case decl: JavaClassDeclaration[G] if decl.isStatic =>
            Referrable.from(decl)
        }.flatten
      case RefEnum(enum) => enum.constants.map(RefEnumConstant(Some(enum), _))
      case _ =>
        Nil // PB: I guess? Maybe we should support "ghost static importing" adt functions and whatnot :)
    }

  def findStaticMember[G](
      javaTypeName: JavaTypeNameTarget[G],
      name: String,
  ): Option[Referrable[G]] = getStaticMembers(javaTypeName).find(_.name == name)

  case class WrongTypeForDefaultValue(t: Type[_]) extends UserError {
    override def code: String = "wrongDefaultType"
    override def text: String =
      t.o.messageInContext(
        s"The type `$t` has no defined default value in VerCors."
      )
  }

  def zeroValue[G](t: Type[G]): Expr[G] =
    t match {
      case TArray(_) => Null()
      case TPointer(_) => Null()
      case TSeq(element) => LiteralSeq(element, Nil)
      case TSet(element) => LiteralSet(element, Nil)
      case TBag(element) => LiteralBag(element, Nil)
      case TOption(_) => OptNone()
      case TMap(key, value) => LiteralMap(key, value, Nil)
      case TVoid() => Void()
      case TNull() => Null()
      case TBool() => ff
      case TString() => Null()
      case TRef() => Null()
      case TProcess() => EmptyProcess()
      case TInt() => const(0)
      case t: TFloat[G] => const(0)
      case TRational() => const(0)
      case TZFraction() => const(0)
      case TByReferenceClass(_, _) => Null()
      case JavaTClass(_, _) => Null()
      case TEnum(_) => Null()
      case TAnyClass() => Null()

      case t => throw WrongTypeForDefaultValue(t)
    }

  def double[G](implicit o: Origin = DiagnosticOrigin): TFloat[G] =
    TFloats.ieee754_64bit
  def float[G](implicit o: Origin = DiagnosticOrigin): TFloat[G] =
    TFloats.ieee754_32bit
}

sealed trait JavaAnnotationData[G]
case object JavaAnnotationData {

  case object BipTransition {
    def get[G](m: JavaMethod[G]): Seq[BipTransition[G]] =
      m.modifiers.collect {
        case ja @ JavaAnnotationEx(_, _, b: BipTransition[G]) => b
      }
  }
  final case class BipTransition[G](
      portName: String,
      source: JavaBipStatePredicateTarget[G],
      target: JavaBipStatePredicateTarget[G],
      guardText: Option[String],
      guard: Option[Expr[G]],
      requires: Expr[G],
      ensures: Expr[G],
  )(implicit val o: Origin)
      extends JavaAnnotationData[G]

  case object BipInvariant {
    def get[G](jc: JavaClassOrInterface[G]): Option[BipInvariant[G]] =
      jc.modifiers.collect {
        case ja @ JavaAnnotation(_, _) if ja.data.isDefined => ja.data.get
      }.collectFirst { case bi: BipInvariant[G] => bi }

  }
  final case class BipInvariant[G](expr: Expr[G]) extends JavaAnnotationData[G]

  case object BipPort {
    def getAll[G](c: JavaClass[G]): Seq[BipPort[G]] =
      c.modifiers.collect { case JavaAnnotationEx(_, _, bp: BipPort[G]) => bp }
  }
  final case class BipPort[G](name: String, portType: BipPortType[G])(
      implicit val o: Origin
  ) extends JavaAnnotationData[G]

  case object BipComponent {
    def get[G](jc: JavaClassOrInterface[G]): Option[BipComponent[G]] =
      jc.modifiers.collect {
        case ja @ JavaAnnotation(_, _) if ja.data.isDefined => ja.data.get
      }.collectFirst { case bct: BipComponent[G] => bct }
  }
  final case class BipComponent[G](
      name: String,
      initial: JavaBipStatePredicateTarget[G],
  ) extends JavaAnnotationData[G]

  case object BipStatePredicate {
    def getAll[G](c: JavaClass[G]): Seq[BipStatePredicate[G]] =
      c.modifiers.collect {
        case JavaAnnotationEx(_, _, bsp: BipStatePredicate[G]) => bsp
      }
  }
  final case class BipStatePredicate[G](name: String, expr: Expr[G])(
      implicit val o: Origin
  ) extends JavaAnnotationData[G]

  case object BipData {
    def get[G](node: Node[G]): Option[BipData[G]] = {
      val modifiers =
        node match {
          case p: JavaParam[G] => p.modifiers
          case m: JavaMethod[G] => m.modifiers
          case _ => return None
        }
      modifiers.collectFirst { case JavaAnnotationEx(_, _, d @ BipData(_)) =>
        d.asInstanceOf[BipData[G]]
      }
    }
  }
  final case class BipData[G](name: String)(implicit val o: Origin)
      extends JavaAnnotationData[G]

  case object BipGuard {
    def get[G](m: JavaMethod[G]): Option[JavaAnnotation[G]] =
      m.modifiers.collect { case ja @ JavaAnnotation(_, _) => (ja, ja.data) }
        .collectFirst { case (ja, Some(_: BipGuard[G])) => ja }

    def getName[G](method: JavaMethod[G]): Option[Expr[G]] =
      method.modifiers.collectFirst {
        case ann: JavaAnnotation[G] if isBip(ann, "Guard") => ann.expect("name")
      }
  }
  final case class BipGuard[G](name: String) extends JavaAnnotationData[G]

  case object BipPure {
    def isPure[G](m: JavaMethod[G]): Boolean =
      m.modifiers.collectFirst { case ja @ JavaAnnotationEx(_, _, BipPure()) =>
        ja
      }.isDefined
  }
  final case class BipPure[G]() extends JavaAnnotationData[G]
}
