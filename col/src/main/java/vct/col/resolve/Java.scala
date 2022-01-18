package vct.col.resolve

import hre.util.FuncTools
import vct.col.origin._
import vct.result.VerificationResult
import vct.col.ast.{ApplicableContract, Block, ClassDeclaration, Expr, JavaClass, JavaClassOrInterface, JavaConstructor, JavaFields, JavaFinal, JavaImport, JavaInterface, JavaMethod, JavaName, JavaNamedType, JavaNamespace, JavaStatic, JavaTClass, TAny, TArray, TBool, TChar, TFloat, TInt, TModel, TNotAValue, TVoid, Type, UnitAccountedPredicate, Variable}
import vct.col.ref.Ref
import vct.result.VerificationResult.Unreachable
import vct.col.util.AstBuildHelpers._

import java.lang.reflect.{Modifier, Parameter}
import scala.collection.mutable

case object Java {
  case class JavaSystemOrigin(preferredName: String) extends Origin {
    override def messageInContext(message: String): String =
      s"At: [Class loaded from JRE with reflection]\n$message"
  }

  private implicit val o: Origin = DiagnosticOrigin
  def JAVA_LANG_OBJECT[G]: JavaNamedType[G] = JavaNamedType(Seq(("java", None), ("lang", None), ("Object", None)))

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
         decls = Seq((field.getName, 0, None)),
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

  def findJavaName[G](name: String, ctx: ReferenceResolutionContext[G]): Option[JavaNameTarget[G]] =
    ctx.stack.flatten.collectFirst {
      case target: JavaNameTarget[G] if target.name == name => target
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
      case ref: RefInstanceFunction[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefInstanceMethod[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefInstancePredicate[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
    }

  def findMethod[G](obj: Expr[G], method: String, args: Seq[Expr[G]], blame: Blame[BuiltinError]): Option[JavaInvocationTarget[G]] =
    (obj.t match {
      case TModel(ref) => ref.decl.declarations.flatMap(Referrable.from).collectFirst {
        case ref: RefModelAction[G] if ref.name == method => ref
        case ref: RefModelProcess[G] if ref.name == method => ref
      }
      case JavaTClass(Ref(cls), Nil) => findMethodInClass(cls, method, args)
      case _ => None
    }).orElse(Spec.builtinInstanceMethod(obj, method, blame))

  def findMethod[G](ctx: ReferenceResolutionContext[G], method: String, args: Seq[Expr[G]]): Option[JavaInvocationTarget[G]] =
    ctx.stack.flatten.collectFirst {
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
}
