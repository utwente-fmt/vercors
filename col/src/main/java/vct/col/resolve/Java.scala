package vct.col.resolve

import hre.util.FuncTools
import vct.col.origin._
import vct.result.VerificationResult
import vct.col.ast.{ApplicableContract, Block, Expr, JavaClass, JavaClassOrInterface, JavaConstructor, JavaFields, JavaImport, JavaInterface, JavaMethod, JavaName, JavaNamedType, JavaNamespace, JavaStatic, JavaTClass, TAny, TArray, TBool, TChar, TFloat, TInt, TModel, TNotAValue, TVoid, Type, Variable}
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
  val JAVA_LANG_OBJECT: JavaNamedType = JavaNamedType(Seq(("java", None), ("lang", None), ("Object", None)))

  def findLoadedJavaTypeName(potentialFQName: Seq[String], ctx: TypeResolutionContext): Option[JavaTypeNameTarget] = {
    (ctx.stack.last ++ ctx.externallyLoadedElements.flatMap(Referrable.from)).foreach {
      case RefJavaNamespace(ns: JavaNamespace) =>
        for(decl <- ns.declarations) {
          Referrable.from(decl).foreach {
            case target: JavaTypeNameTarget =>
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

  private val currentlyLoading = mutable.Map[Seq[String], mutable.ArrayBuffer[JavaNamedType]]()

  def lazyType(name: Seq[String], ctx: TypeResolutionContext): JavaNamedType = {
    val result = JavaNamedType(name.map((_, None)))

    currentlyLoading.get(name) match {
      case Some(lazyQueue) => lazyQueue += result
      case None => result.ref = Some(findJavaTypeName(name, ctx).getOrElse(
        throw Unreachable(s"Invalid JRE: The type `${name.mkString(".")}` built in to the JRE could not be loaded.")))
    }

    result
  }

  def findRuntimeJavaType(potentialFQName: Seq[String], ctx: TypeResolutionContext): Option[JavaClassOrInterface] = {
    if(currentlyLoading.contains(potentialFQName))
      throw Unreachable("Aborting cyclic loading of classes from Java runtime")

    implicit val o: Origin = JavaSystemOrigin("unknown_jre")
    currentlyLoading(potentialFQName) = mutable.ArrayBuffer()

//    println(s"[warning] No specification was found for class ${potentialFQName.mkString(".")}, so a shim will be loaded from the JRE.")

    try {
      val classLoader = this.getClass.getClassLoader
      val cls = classLoader.loadClass(potentialFQName.mkString("."))

      val colClass = translateRuntimeClass(cls)(o, ctx)

      ctx.externallyLoadedElements += new JavaNamespace(Some(JavaName(potentialFQName.init)), Nil, Seq(colClass))

      for(t <- currentlyLoading.remove(potentialFQName).get) {
        t.ref = Some(RefJavaClass(colClass))
      }

      Some(colClass)
    } catch {
      case _: ClassNotFoundException =>
        currentlyLoading.remove(potentialFQName)
        None
    }
  }

  def translateRuntimeType(t: Class[_])(implicit o: Origin, ctx: TypeResolutionContext): Type = t match {
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

  def translateRuntimeParameter(param: Parameter)(implicit o: Origin, ctx: TypeResolutionContext): Variable = {
    new Variable(translateRuntimeType(param.getType))(SourceNameOrigin(param.getName, o))
  }

  def translateRuntimeClass(cls: Class[_])(implicit o: Origin, ctx: TypeResolutionContext): JavaClassOrInterface = {
    val cons = cls.getConstructors.map(cons => {
      new JavaConstructor(
        modifiers = Nil,
        name = cls.getSimpleName,
        parameters = cons.getParameters.toIndexedSeq.map(translateRuntimeParameter),
        typeParameters = Nil,
        signals = Nil,
        body = Block(Nil),
        contract = ApplicableContract(tt, tt, tt, Nil, Nil, Nil),
      )
    })

    val methods = cls.getMethods.map(method => {
      new JavaMethod(
        modifiers = if((method.getModifiers & Modifier.STATIC) != 0) Seq(JavaStatic()) else Nil,
        returnType = translateRuntimeType(method.getReturnType),
        dims = 0,
        name = method.getName,
        parameters = method.getParameters.toIndexedSeq.map(translateRuntimeParameter),
        typeParameters = Nil,
        signals = Nil,
        body = None,
        contract = ApplicableContract(tt, tt, tt, Nil, Nil, Nil),
      )(AbstractApplicable)(SourceNameOrigin(method.getName, o))
    })

    val fields = cls.getFields.map(field => {
       new JavaFields(
         modifiers = if((field.getModifiers & Modifier.STATIC) != 0) Seq(JavaStatic()) else Nil,
         t = translateRuntimeType(field.getType),
         decls = Seq((field.getName, 0, None)),
       )
    })

    if(cls.isInterface) {
      new JavaInterface(
        name = cls.getName.split('.').last,
        modifiers = Nil,
        typeParams = Nil,
        ext = cls.getInterfaces.toIndexedSeq.map(cls => lazyType(cls.getName.split('.').toIndexedSeq, ctx)),
        decls = fields.toIndexedSeq ++ cons.toIndexedSeq ++ methods.toIndexedSeq,
      )(SourceNameOrigin(cls.getName.split('.').last, o))
    } else {
      new JavaClass(
        name = cls.getName.split('.').last,
        modifiers = Nil,
        typeParams = Nil,
        intrinsicLockInvariant = `tt`,
        ext = Option(cls.getSuperclass).map(cls => lazyType(cls.getName.split('.').toIndexedSeq, ctx)).getOrElse(TAny()),
        imp = cls.getInterfaces.toIndexedSeq.map(cls => lazyType(cls.getName.split('.').toIndexedSeq, ctx)),
        decls = fields.toIndexedSeq ++ cons.toIndexedSeq ++ methods.toIndexedSeq,
      )(SourceNameOrigin(cls.getName.split('.').last, o))
    }
  }

  def findJavaTypeName(names: Seq[String], ctx: TypeResolutionContext): Option[JavaTypeNameTarget] = {
    val namespace = ctx.namespace.getOrElse(throw VerificationResult.Unreachable("A JavaClass declared outside a JavaNamespace is invalid."))
    val potentialFQNames: Seq[Seq[String]] = names match {
      case Seq(singleName) =>
        val inThisPackage = namespace.pkg match {
          case Some(pkg) => pkg.names :+ singleName
          case None => Seq(singleName)
        }
        val fromImport = namespace.imports.collect {
          case JavaImport(false, name, /*star = */ true) => name.names :+ singleName
          case JavaImport(false, name, /*star = */ false) if name.names.last == singleName => name.names
        }
        val fromPredef = Seq("java", "lang", singleName)
        fromImport :+ inThisPackage :+ fromPredef
      case moreNames => Seq(moreNames)
    }

    FuncTools.firstOption(potentialFQNames, findLoadedJavaTypeName(_, ctx))
      .orElse(FuncTools.firstOption(potentialFQNames, findRuntimeJavaType(_, ctx)).map(RefJavaClass))
  }

  def findJavaName(name: String, ctx: ReferenceResolutionContext): Option[JavaNameTarget] =
    ctx.stack.flatten.collectFirst {
      case target: JavaNameTarget if target.name == name => target
    }

  def findDeref(obj: Expr, name: String, ctx: ReferenceResolutionContext, blame: Blame[BuiltinError]): Option[JavaDerefTarget] =
    (obj.t match {
      case t: TNotAValue => t.decl.get match {
        case RefUnloadedJavaNamespace(pkg) =>
          Some(findJavaTypeName(pkg :+ name, ctx.asTypeResolutionContext)
            .getOrElse(RefUnloadedJavaNamespace(pkg :+ name)))
        case RefJavaClass(decl) =>
          decl.decls.flatMap(Referrable.from).collectFirst {
            case ref @ RefJavaField(decls, idx) if ref.name == name && ref.decls.modifiers.contains(JavaStatic()) => ref
          }
        case _ => None
      }
      case TModel(Ref(model)) => model.declarations.flatMap(Referrable.from).collectFirst {
        case ref @ RefModelField(_) if ref.name == name => ref
      }
      case JavaTClass(Ref(cls), _) => cls.decls.flatMap(Referrable.from).collectFirst {
        case ref @ RefJavaField(_, _) if ref.name == name && !ref.decls.modifiers.contains(JavaStatic()) => ref
      }
      case _ => None
    }).orElse(Spec.builtinField(obj, name, blame))

  def findMethodInClass(cls: JavaClassOrInterface, method: String, args: Seq[Expr]): Option[JavaInvocationTarget] =
    cls.decls.flatMap(Referrable.from).collectFirst {
      case ref: RefJavaMethod if ref.name == method && Util.compat(args, ref.decl.parameters) => ref
      case ref: RefInstanceFunction if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefInstanceMethod if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefInstancePredicate if ref.name == method && Util.compat(args, ref.decl.args) => ref
    }

  def findMethod(obj: Expr, method: String, args: Seq[Expr], blame: Blame[BuiltinError]): Option[JavaInvocationTarget] =
    (obj.t match {
      case TModel(ref) => ref.decl.declarations.flatMap(Referrable.from).collectFirst {
        case ref: RefModelAction if ref.name == method => ref
        case ref: RefModelProcess if ref.name == method => ref
      }
      case JavaTClass(Ref(cls), Nil) => findMethodInClass(cls, method, args)
      case _ => None
    }).orElse(Spec.builtinInstanceMethod(obj, method, blame))

  def findMethod(ctx: ReferenceResolutionContext, method: String, args: Seq[Expr]): Option[JavaInvocationTarget] =
    ctx.stack.flatten.collectFirst {
      case ref: RefJavaMethod if ref.name == method && Util.compat(args, ref.decl.parameters) => ref
      case ref: RefInstanceFunction if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefInstanceMethod if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefInstancePredicate if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefFunction if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefProcedure if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefPredicate if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefADTFunction if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefModelProcess if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefModelAction if ref.name == method && Util.compat(args, ref.decl.args) => ref
    }
}
